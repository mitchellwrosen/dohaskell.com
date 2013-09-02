# Database

The site runs off the [Persistent ORM](http://hackage.haskell.org/package/persistent-1.2.3.0). Interacting with the
database is simple.

For each datatype specified in config/models, there will be an equivalent Dao module (e.g. User.Dao). The Dao is the
first layer on top of Persistent, and consists simply of useful lookups, inserts, etc. Note that the Dao modules are not
only database agnostic, they are **PersitMonadBackend agnostic** as well. Dao functions thus should all have type
signature ```PersistMonadBackend m => ... -> ... -> m ()``` (or something other than unit) and should **not** be in
```HandlerT site IO a``` (aka ```Handler```).

In this way, the Dao is extensible and reusable by components of dohaskell.com that are not Yesod itself.

# The Yesod layer

There is a layer between the Dao and ```Handler``` from Yesod. For now, it consists of Dao functions wrapped in
```runDB``` calls. For lack of a better term, the modules will be called ```Ydao```. The Ydao should reuse names from
the Dao, and use a qualified import: ```import qualified HaskFunction.Dao as D```.

[Home](home.html)
