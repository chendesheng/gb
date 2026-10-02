#import <AppKit/AppKit.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#include <stdlib.h>
#include <string.h>

// Included once by inline-c-objc's generated Objective-C translation unit.
static NSMutableArray<NSString *> *gbActions;
static NSMenuItem *gbPowerOnItem;
static NSMenuItem *gbPowerOffItem;
static BOOL gbPickerOpen;

@interface GBCartridgeMenuTarget : NSObject
- (void)openCartridge:(id)sender;
- (void)powerOn:(id)sender;
- (void)powerOff:(id)sender;
@end

@implementation GBCartridgeMenuTarget
- (void)openCartridge:(id)sender {
    if (gbPickerOpen) return;
    gbPickerOpen = YES;
    NSOpenPanel *panel = [NSOpenPanel openPanel];
    panel.title = @"Open Cartridge";
    panel.prompt = @"Open";
    panel.canChooseDirectories = NO;
    panel.allowsMultipleSelection = NO;
    panel.allowedContentTypes = @[[UTType typeWithFilenameExtension:@"bin"]];
    panel.allowsOtherFileTypes = NO;
    [panel beginWithCompletionHandler:^(NSModalResponse result) {
        gbPickerOpen = NO;
        if (result == NSModalResponseOK) {
            [gbActions addObject:[@"O" stringByAppendingString:panel.URL.path]];
        }
    }];
}
- (void)powerOn:(id)sender { [gbActions addObject:@"P"]; }
- (void)powerOff:(id)sender { [gbActions addObject:@"F"]; }
@end

static void gbInstallMenu(void) {
    @autoreleasepool {
        gbActions = [[NSMutableArray alloc] init];
        GBCartridgeMenuTarget *target = [[GBCartridgeMenuTarget alloc] init];
        NSMenu *menu = [[NSMenu alloc] initWithTitle:@"File"];
        menu.autoenablesItems = NO;
        NSMenuItem *open = [[NSMenuItem alloc] initWithTitle:@"Open Cartridge…"
            action:@selector(openCartridge:) keyEquivalent:@"o"];
        open.target = target;
        [menu addItem:open];
        [open release];
        [menu addItem:[NSMenuItem separatorItem]];
        gbPowerOnItem = [[NSMenuItem alloc] initWithTitle:@"Power On"
            action:@selector(powerOn:) keyEquivalent:@""];
        gbPowerOnItem.target = target;
        gbPowerOnItem.enabled = NO;
        [menu addItem:gbPowerOnItem];
        gbPowerOffItem = [[NSMenuItem alloc] initWithTitle:@"Power Off"
            action:@selector(powerOff:) keyEquivalent:@""];
        gbPowerOffItem.target = target;
        gbPowerOffItem.enabled = NO;
        [menu addItem:gbPowerOffItem];
        NSMenuItem *file = [[NSMenuItem alloc] initWithTitle:@"File" action:NULL keyEquivalent:@""];
        file.submenu = menu;
        [NSApp.mainMenu insertItem:file atIndex:1];
        [file release];
        [menu release];
        // Retain target and power items for the lifetime of the app.
    }
}

static char *gbTakeAction(void) {
    @autoreleasepool {
        if (gbActions.count == 0) return NULL;
        char *result = strdup(gbActions.firstObject.UTF8String);
        [gbActions removeObjectAtIndex:0];
        return result; // Caller frees the UTF-8 copy.
    }
}

static void gbSetPowerState(int loaded, int powered) {
    gbPowerOnItem.enabled = loaded && !powered;
    gbPowerOffItem.enabled = powered;
}

static void gbShowError(const char *message) {
    @autoreleasepool {
        NSAlert *alert = [[NSAlert alloc] init];
        alert.messageText = @"Game Boy";
        alert.informativeText = [NSString stringWithUTF8String:message];
        [alert addButtonWithTitle:@"OK"];
        [alert runModal];
        [alert release];
    }
}
