#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
  // For now, there are a finite number of cells. 1024 should be more than enough in most cases...
  short data[1024];
  unsigned int p = 0;
  // The terminal device normally waits for newlines by default, so
  // change change it to send each character to stdin immediately.
  static struct termios oldt, newt;
  tcgetattr(STDIN_FILENO, &oldt);
  newt = oldt;
  newt.c_lflag &= ~(ICANON);
  tcsetattr(STDIN_FILENO, TCSANOW, &newt);
  
  memset(data, 0, sizeof(data));

  /// --- BF CODE --- ///
  
  // Restore old terminal settings
  tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
  
  return 0;
}