#include "stub.h"
#include "ta_libc.h"
#include "ta_abstract.h"

int add(int a, int b) {
    return a + b;
}

TA_LIB_API TA_RetCode TA_GroupTableAlloc( TA_StringTable **table );
TA_LIB_API TA_RetCode TA_GroupTableFree ( TA_StringTable *table );
