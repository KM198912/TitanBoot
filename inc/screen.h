// screen.h - VGA text mode screen functions

#ifndef SCREEN_H
#define SCREEN_H

#define VGA_ADDRESS 0xB8000
#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_COLOR 0x0F  // White on black

// Function declarations
void screen_clear(void);
void screen_print(const char* str);
void screen_put_char(char c, int x, int y);

// Global cursor position
static int cursor_x = 0;
static int cursor_y = 0;

// Clear the screen
void screen_clear(void) {
    unsigned short* vga = (unsigned short*)VGA_ADDRESS;
    unsigned short blank = (VGA_COLOR << 8) | ' ';
    
    for (int i = 0; i < VGA_WIDTH * VGA_HEIGHT; i++) {
        vga[i] = blank;
    }
    
    cursor_x = 0;
    cursor_y = 0;
}

// Print a character at specific position
void screen_put_char(char c, int x, int y) {
    unsigned short* vga = (unsigned short*)VGA_ADDRESS;
    unsigned short attribute = VGA_COLOR << 8;
    int offset = y * VGA_WIDTH + x;
    vga[offset] = attribute | c;
}

// Print a string
void screen_print(const char* str) {
    unsigned short* vga = (unsigned short*)VGA_ADDRESS;
    unsigned short attribute = VGA_COLOR << 8;
    
    while (*str) {
        if (*str == '\n') {
            cursor_x = 0;
            cursor_y++;
        } else {
            int offset = cursor_y * VGA_WIDTH + cursor_x;
            vga[offset] = attribute | *str;
            cursor_x++;
            
            if (cursor_x >= VGA_WIDTH) {
                cursor_x = 0;
                cursor_y++;
            }
        }
        
        // Scroll if needed
        if (cursor_y >= VGA_HEIGHT) {
            // Simple scroll - just wrap to top (real OS would shift lines up)
            cursor_y = 0;
        }
        
        str++;
    }
}

#endif // SCREEN_H