#include "test.h"

int main() {
    ASSERT(4, __LINE__);  // don't move me!
    
    int before = __LINE__;
    // foo this is all \
    on one line \
    this too is part of the comment
    int after = __LINE__;
    ASSERT(4, after - before);
    
    // clang disagrees about if we're setting the current line or the next line. 
#line 500 "foo"
    ASSERT(501, __LINE__);
    ASSERT(0, strcmp(__FILE__, "foo"));
    
#define THE_LINE __LINE__  // line of the expansion point is used, not this one 
    
#line 800 "bar"
    ASSERT(801, __LINE__);
    ASSERT(0, strcmp(__FILE__, "bar"));

#line 1
    ASSERT(2, __LINE__);
    ASSERT(3, THE_LINE);

// TODO: clang doesn't like this. should we disallow too? 
# 200 "xyz" 2 3
    ASSERT(201, __LINE__);
    ASSERT(0, strcmp(__FILE__, "xyz"));

    printf("OK\n");
    return 0;
}
