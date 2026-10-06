use v5.45;
use warnings;
use Plack::Handler::Feersum;
use Router::Ragel;

use constant text => [qw'Content-Type text/plain'];

my $hdr = text;
my $res_200 = [200, $hdr, ['']];
my $res_404 = [404, $hdr, ['']];

sub cpu_count {
    if (open my $fh, '<', '/sys/fs/cgroup/cpu.max') {
        my ($quota, $period) = split ' ', (<$fh> // '');
        if ($quota && $period && $quota ne 'max' && $period > 0) {
            my $n = int($quota / $period);
            return $n if $n >= 1;
        }
    }
    if (open my $qf, '<', '/sys/fs/cgroup/cpu/cpu.cfs_quota_us') {
        my $quota = <$qf> // 0;
        if (open my $pf, '<', '/sys/fs/cgroup/cpu/cpu.cfs_period_us') {
            my $period = <$pf> // 0;
            if ($quota > 0 && $period > 0) {
                my $n = int($quota / $period);
                return $n if $n >= 1;
            }
        }
    }
    if (open my $fh, '<', '/proc/self/status') {
        while (<$fh>) {
            if (/^Cpus_allowed_list:\s*(\S+)/) {
                my $n = 0;
                for (split /,/, $1) {
                    $n += /^(\d+)-(\d+)$/ ? $2 - $1 + 1 : 1;
                }
                return $n if $n >= 1;
            }
        }
    }
    return `nproc` + 0 || 1;
}

my $fallback = sub ($env, @) {
    return $res_404;
};

my $router = Router::Ragel->new
    ->add('/', sub ($env, @) {
        return $res_200 if $env->{REQUEST_METHOD} eq 'GET';
        return $res_404;
    })
    ->add('/user', sub ($env, @) {
        return $res_200 if $env->{REQUEST_METHOD} eq 'POST';
        return $res_404;
    })
    ->add('/user/:id', sub ($env, $id) {
        return [200, $hdr, [$id]] if $env->{REQUEST_METHOD} eq 'GET';
        return $res_404;
    })
    ->compile;

Plack::Handler::Feersum->new(
    listen              => ['0.0.0.0:3000'],
    pre_fork            => cpu_count(),
    reuseport           => 1,
    quiet               => 1,
    keepalive           => 1,
    max_connection_reqs => 0,
)->run(sub ($env) {
    my ($handler, @cap) = Router::Ragel::match($router, $env->{PATH_INFO});
    return ($handler // $fallback)->($env, @cap);
});
