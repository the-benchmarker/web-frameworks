use v5.40;
use warnings;
use Feersum::Runner;
use Router::Ragel;

use constant text => [qw'Content-Type text/plain'];

my $empty_body = '';

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

my $fallback = sub ($h, @) {
    return $h->send_response(404, text, $empty_body);
};

my $router = Router::Ragel->new
    ->add('/', sub ($h, @) {
        return $h->send_response(200, text, $empty_body) if $h->method eq 'GET';
        return $h->send_response(404, text, $empty_body);
    })
    ->add('/user', sub ($h, @) {
        return $h->send_response(200, text, $empty_body) if $h->method eq 'POST';
        return $h->send_response(404, text, $empty_body);
    })
    ->add('/user/:id', sub ($h, $id) {
        return $h->send_response(200, text, $id) if $h->method eq 'GET';
        return $h->send_response(404, text, $empty_body);
    })
    ->compile;

Feersum::Runner->new(
    listen              => ['0.0.0.0:3000'],
    pre_fork            => cpu_count(),
    reuseport           => 1,
    quiet               => 1,
    keepalive           => 1,
    max_connection_reqs => 0,
)->run(sub ($h) {
    my ($handler, @cap) = Router::Ragel::match($router, $h->path);
    return ($handler // $fallback)->($h, @cap);
});
