// vim: set et ts=4 sw=4 list :

// No code here. I did this one entirely in vim:

// Step 1: remove the patterns:
//      :%s/.* | //

// Step 2: replace all the characters with x:
//      :%s/[a-g]/x/g

// Step 3: substitute for the digits we want, eliminate the ones we don't:
//      :%s/xxxxxxx/8/g
//      :%s/xxxxxx//g
//      :%s/xxxxx//g
//      :%s/xxxx/4/g
//      :%s/xxx/7/g
//      :%s/xx/1/g
// (order matters here)

// Step 4: Jackhammer the J key until everything is on one line. (There's
// probably a more efficient way to do this, but :shrug:.)
//      JJJJJJ...

// Step 5: Remove all whitespace.
//      :%s/ //g

// Step 6: Go to the end of the one remaining line and look at the ruler
// in the modeline.
//      $
