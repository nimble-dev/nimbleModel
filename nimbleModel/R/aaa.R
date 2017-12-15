## This file is named aaa.R because it should be loaded first.
## Loading is actually controlled by DESCRIPTION, and we do not
## assume "aaa" comes first in all locales.  But it is convenient
## to the authors.

nimbleUserNamespace <- as.environment(list(sessionSpecificDll = NULL)) 
# This is constructed as given instead of with simply "new.env()" because "new.env()" here fails with: Error in as.environment(pos) : using 'as.environment(NULL)' is defunct when testing package loading during INSTALL.
