

[![Criollo](https://criollo.io/res/doc/images/criollo-github.png)](https://criollo.io/)

#### A powerful Cocoa based web application framework for OS X and iOS.

[![Version Status](https://img.shields.io/cocoapods/v/Criollo.svg?style=flat)](http://cocoadocs.org/docsets/Criollo)  [![Platform](http://img.shields.io/cocoapods/p/Criollo.svg?style=flat)](http://cocoapods.org/?q=Criollo) [![Carthage compatible](https://img.shields.io/badge/Carthage-compatible-4BC51D.svg?style=flat)](https://github.com/Carthage/Carthage)
 [![MIT License](https://img.shields.io/badge/license-MIT-orange.svg?style=flat)](https://opensource.org/licenses/MIT) [![Twitter](https://img.shields.io/badge/twitter-@Criolloio-orange.svg?style=flat)](http://twitter.com/Criolloio)


Criollo helps create really fast standalone web apps that deliver content directly over HTTP or FastCGI. You can write code in Objective-C or Swift. And you can use technologies you know and love: Grand Central Dispatch, NSURLSession, CoreImage and many more. 

It's as easy as this:

```swift
let server = CRHTTPServer()
server.get("/") { (request, response, completionHandler) in
	response.send("Hello world!")
}
server.startListening()
```

and in Objective-C:

```objective-c
CRServer* server = [[CRHTTPServer alloc] init];
[server get:@"/" block:^(CRRequest * request, CRResponse * response, CRRouteCompletionBlock completionHandler) {
    [response send:@"Hello world!"];
}];
[server startListening];
```

## Why?

Criollo was created in order to take advantage of the truly awesome tools and APIs that OS X and iOS provide and serve content produced with them over the web. 

It incorporates an HTTP web server and a [FastCGI](https://fast-cgi.github.io/) application server that are used to deliver content. The server is built on Grand Central Dispatch and designed for *speed*.

## How to Use

Criollo can easily be embedded as a web-server inside your OS X or iOS app, should you be in need of such a feature, however it was designed to create standalone, long-lived daemon style apps. It is fully [`launchd`](http://launchd.info/) compatible and replicates the lifecycle and behavior of `NSApplication`, so that the learning curve should be as smooth as possible. 

For a more real-world example, check out the [criollo.io](https://criollo.io) website, made using Criollo and available for your cloning pleasure at [https://github.com/thecatalinstan/Criollo-Web](https://github.com/thecatalinstan/Criollo-Web).

See the [Hello World Multi Target example](https://github.com/thecatalinstan/Criollo/tree/master/Examples/HelloWorld-MultiTarget) for a demo of the two usage patterns.

## Getting Started

- [Download Criollo](https://github.com/thecatalinstan/Criollo/archive/master.zip) and try out the included OS X and iOS [example apps](https://github.com/thecatalinstan/Criollo/Examples). *Criollo requires [CocoaAsyncSocket](https://github.com/robbiehanson/CocoaAsyncSocket), so do not forget to [download](https://github.com/robbiehanson/CocoaAsyncSocket/archive/master.zip) it into `Libraries/CocoaAsyncSocket`*.
- Read the [“Getting Started” guide](https://github.com/thecatalinstan/Criollo/wiki/Getting-Started) and move further with the [“Doing More Stuff” guide](https://github.com/thecatalinstan/Criollo/wiki/Doing-More-Stuff)
- Check out the [documentation](http://cocoadocs.org/docsets/Criollo/) for a look at the APIs available
- Learn how to deploy your Criollo apps in the [“Deployment” guide](https://github.com/thecatalinstan/Criollo/wiki/Deployment)

## Installing

The preferred way of installing Criollo is through [CocoaPods](http://cocoapods.org). However, you can also embed the framework in your projects manually.

### Installing with CocoaPods

1. Create the Podfile if you don’t already have one. You can do so by running `pod init` in the folder of the project.
2. Add Criollo to your Podfile. `pod 'Criollo', '~> 0.4’`
3. Run `pod install`

Please note that Criollo will download [CocoaAsyncSocket](https://github.com/robbiehanson/CocoaAsyncSocket) as a dependency.

### Cloning the repo

Criollo uses [CocoaAsyncSocket](https://github.com/robbiehanson/CocoaAsyncSocket) which is included as a git submodule

```bashh
git clone --recursive https://github.com/thecatalinstan/Criollo.git
```

## Work in Progress

Criollo is work in progress and - as such - it’s not ready for the wild yet. The reason for this is mainly missing functionality and sheer lack of documentation[^It is also very high on my list of priorities, but sadly still a “to-do” item].

The existing APIs are relatively stable and are unlikely to change dramatically unless marked as such.

## Get in Touch

If you have any **questions** regarding the project or **how to** do anything with it, please feel free to get in touch either on Twitter [@criolloio](https://twitter.com/criolloio) or by plain old email [criollo@criollo.io](mailto:criollo@criollo.io).

I really encourage you to [submit an issue](https://github.com/thecatalinstan/Criollo/issues/new), as your input is really and truly appreciated.
