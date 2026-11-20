//
//  main.m
//  server_objc_criollo
//
//  Created by Cătălin Stan on 25/10/2017.
//  Copyright © 2017 Cătălin Stan. All rights reserved.
//

#import "AppDelegate.h"
#import <Criollo/Criollo.h>

int main(int argc, const char *argv[]) {
  return CRApplicationMain(argc, argv, AppDelegate.new);
}
