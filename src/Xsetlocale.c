/*   From Frederick Jones                        Mar 6, 98
 RedHat have managed to break Motif in their release 5.
 Applications will not link because of a missing routine
 _Xsetlocale.

 The work-around is to compile the following into your application:

 Once the application is linked, the file selection widget will not
 work properly.  A work-around is to link against the previous version
 of libc.so.  For Edgr I did it as follows:

 g77|gcc ...  -lXm -lXt -lX11 -Wl,-rpath,/usr/i486-linux-libc5/lib libc.so.5

 From Renee Poutissou : I tried using the previous version but it did not work 
 for me. Let's use the dummy routine
*/

#undef setlocale
#include <locale.h>

char *_Xsetlocale(int category, const char *name)
{
   return setlocale(category, name);
}
