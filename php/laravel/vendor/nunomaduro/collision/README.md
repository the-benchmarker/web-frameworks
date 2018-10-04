<p align="center">
    <img src="https://raw.githubusercontent.com/nunomaduro/collision/stable/docs/logo.png" alt="Collision logo" width="480">
    <br>
    <img src="https://raw.githubusercontent.com/nunomaduro/collision/stable/docs/example.png" alt="Collision code example" height="300">
</p>

<p align="center">
  <a href="https://travis-ci.org/nunomaduro/collision"><img src="https://img.shields.io/travis/nunomaduro/collision/stable.svg" alt="Build Status"></img></a>
  <a href="https://scrutinizer-ci.com/g/nunomaduro/collision"><img src="https://img.shields.io/scrutinizer/g/nunomaduro/collision.svg" alt="Quality Score"></img></a>
  <a href="https://scrutinizer-ci.com/g/nunomaduro/collision"><img src="https://img.shields.io/scrutinizer/coverage/g/nunomaduro/collision.svg" alt="Coverage"></img></a>
  <a href="https://packagist.org/packages/nunomaduro/collision"><img src="https://poser.pugx.org/nunomaduro/collision/d/total.svg" alt="Total Downloads"></a>
  <a href="https://packagist.org/packages/nunomaduro/collision"><img src="https://poser.pugx.org/nunomaduro/collision/v/stable.svg" alt="Latest Stable Version"></a>
  <a href="https://packagist.org/packages/nunomaduro/collision"><img src="https://poser.pugx.org/nunomaduro/collision/license.svg" alt="License"></a>
</p>

## About Collision

Collision was created by, and is maintained by [Nuno Maduro](https://github.com/nunomaduro), and is an error handler framework for console/command-line PHP applications.

- Build on top of [Whoops](https://github.com/filp/whoops).
- Supports [Laravel](https://github.com/laravel/laravel) Artisan & [PHPUnit](https://github.com/sebastianbergmann/phpunit).
- Built with [PHP 7](https://php.net) using modern coding standards.

## Installation & Usage

> **Requires [PHP 7.1+](https://php.net/releases/)**

Require Collision using [Composer](https://getcomposer.org):

```bash
composer require nunomaduro/collision --dev
```

If you are not using Laravel, you need to register the handler in your code:

```php
(new \NunoMaduro\Collision\Provider)->register();
```

## Phpunit adapter

Phpunit must be 7.0 or higher.

Add the following configuration to your `phpunit.xml`:

```xml
    <listeners>
        <listener class="NunoMaduro\Collision\Adapters\Phpunit\Listener" />
    </listeners>
```

## Contributing

Thank you for considering to contribute to Collision. All the contribution guidelines are mentioned [here](CONTRIBUTING.md).

You can have a look at the [CHANGELOG](CHANGELOG.md) for constant updates & detailed information about the changes. You can also follow the twitter account for latest announcements or just come say hi!: [@enunomaduro](https://twitter.com/enunomaduro)

## Support the development
**Do you like this project? Support it by donating**

- PayPal: [Donate](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=66BYDWAT92N6L)
- Patreon: [Donate](https://www.patreon.com/nunomaduro)

## License

Collision is an open-sourced software licensed under the [MIT license](LICENSE.md).
