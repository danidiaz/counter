Overengineered counter REST API
-------------------------------

This is a trivial REST API which provides counters. Some example curls:

    # Create new counter, return the counter id
    curl --request POST \
    --url http://localhost:8000/counter \
    -v -u "user:password"

    # Increase counter by 1
    curl --request POST \
    --url http://localhost:8000/counter/7e94c7a9-c54a-459d-8b2b-84cce56e30b9/increase \
    -v -u "user:password"

    # Get current value of counter
    curl --request GET \
    --url http://localhost:8000/counter/7e94c7a9-c54a-459d-8b2b-84cce56e30b9 \
    -v -u "user:password"

    curl --request DELETE \
    --url http://localhost:8000/counter/7e94c7a9-c54a-459d-8b2b-84cce56e30b9 \
    -v -u "user:password"

There's no persistence, counters are kept in a map in memory.

Why so overengineered?
----------------------

I wanted to scratch the following itches:

- The Servant API (`Counter.API`) and the model (`Counter.Model`) *should know nothing about each other*. 
    - The connection between the two is made in a third `Counter.Server` module.
        - With the help of utilities form the `Servant.Server.ToHandler` module.
    - The Servant API defines its own data transfer objects which mirror datatypes in the model.
        - So, code duplication and the need for conversion functions.
            - It could potentially help with versioning.
            - Having to declare `FromHttpApiData` instances for the datatypes in you model feels kind of gross, better define them for your DTOs.

- Server handlers should be "built" using dependency injection.
    - Convenient way of wiring dependencies like loggers.
    - The dependency injection context is a good place to add debug traces.
        - It sits on top on the module hierarchy, close to `Main`, so recompilations shouldn't be too painful.
    - Only *direct* dependencies should appear in a component's signature.
        - So `Foo` depends on `Bar`, and `Bar` uses some `BazCache` internally? That's great. But `Foo` should not know *at all* about `BazCache`.
            - Adding `BazCache` to `Bar` and then having to update the signatures of a zillion clients of `Bar`, and of client's clients, is not my idea of fun.
    - I'm using my own library [dep-t](https://hackage.haskell.org/package/dep-t).

- For each logging message emitted *by the model*, I want to print the Servant handler (the handler field name) resposible for triggering it.
    - BUT! Components in the model *should know nothing about handlers, or Servant*. 
        - Only the logger component is allowed to know that handlers exist.

Links
-----

- [Servant documentation](https://docs.servant.dev/en/stable/)
- [servant on Hackage](https://hackage.haskell.org/package/servant)
- [servant-server on Hackage](https://hackage.haskell.org/package/servant-server)
- [Named routes in Servant](https://www.tweag.io/blog/2022-02-24-named-routes/)

- [How to Haskell: Sharing Data Types is Tight Coupling](https://leapyear.io/resources/blog-posts/how-to-haskell-sharing-data-types-is-tight-coupling/). [tweet](https://twitter.com/DiazCarrete/status/1570487241755197440).

- [Mapped Diagnostic Context in Java's Logback loggin framework](https://logback.qos.ch/manual/mdc.html)