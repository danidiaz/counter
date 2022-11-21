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

This curl gives a global view of all components with "control" endpoints:

    curl --request GET \
    --url http://localhost:8000/knob \
    -v -u "user:password"

These curls control the logger:

    # Get current state of the logger
    curl --request GET \
    --url http://localhost:8000/knob/logger \
    -v -u "user:password"

    # Set log level to error
    curl --request POST \
    --url http://localhost:8000/knob/logger \
    -v -u "user:password"
    --header 'Content-Type: application/json' \
    --data '{
        "minimumLevel": "error"
    }'

    # Reset log level to original value
    curl --request DELETE \
    --url http://localhost:8000/knob/logger \
    -v -u "user:password"

Log messages look like this. Notice they include the [names](https://www.tweag.io/blog/2022-02-24-named-routes/) of the servant handlers.

    [nix-shell:~/counter]$ cabal run
    [(CounterCollectionAPI,"create")] withResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] withExistingResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] withResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"query")] withExistingResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"query")] withResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] Requesting counter deletion CounterId cbb1eb12-66d0-42f3-9776-6c756713d281
    [(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] withExistingResource
    [(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] withResource

Counters that haven't been updated in a period of time (specified in the
configuration file) are deleted. This is just an excuse for playing with
asynchronous actions.

Why so overengineered?
----------------------

I wanted to scratch the following itches:

- The Servant API (`Counter.API`) and the model (`Counter.Model`) *should know nothing about each other*. 
    - The connection between the two is made in a third `Counter.Server` module.
        - With the help of utilities form the `Servant.Server.ToHandler` module.
    - The Servant API defines its own data transfer objects which mirror datatypes in the model.
        - So, code duplication and the need for conversion functions.
            - It could potentially help with versioning.
            - It could potentially help when generating Servant APIs from OpenAPI specs.
            - Having to declare `FromHttpApiData` instances for the datatypes in you model feels kind of gross, better define them for your DTOs.

- Servant handlers should be "built" using dependency injection.
    - Convenient way of wiring dependencies like loggers.
    - The dependency injection context is a good place to add debug traces.
        - It sits on top on the module hierarchy, close to `Main`, so recompilations shouldn't be too painful.
        - It's a place where all types are known and concrete, so their `Show`/`ToJSON` instances are available.
    - Only *direct* dependencies should appear in a component's signature.
        - So `Foo` depends on `Bar`, and `Bar` uses some `BazCache` internally? That's great. But `Foo` should not know *at all* about `BazCache`.
            - Adding `BazCache` to `Bar` and then having to update the signatures of a zillion clients of `Bar`, and of client's clients, is not my idea of fun.
    - I'm using my own library [dep-t](https://hackage.haskell.org/package/dep-t).

- For each logging message emitted *by the model*, I want to print the Servant handler (the handler field name) resposible for triggering it.
    - BUT! Components in the model *should know nothing about handlers, or Servant*. 
        - Only the logger component is allowed to know that handlers exist.

- I aslo want to tag each logging message with the componnet that emitted it.
Preferably, this should be done automatically.

- Each component should be in charge of parsing its *own* piece of the
configuration. When wiring components together, the different configuration
parsers are aggregated.

Links
-----

- [Servant documentation](https://docs.servant.dev/en/stable/)
- [servant on Hackage](https://hackage.haskell.org/package/servant)
- [servant-server on Hackage](https://hackage.haskell.org/package/servant-server)
- [Named routes in Servant](https://www.tweag.io/blog/2022-02-24-named-routes/)

- [How to Haskell: Sharing Data Types is Tight Coupling](https://leapyear.io/resources/blog-posts/how-to-haskell-sharing-data-types-is-tight-coupling/). [tweet](https://twitter.com/DiazCarrete/status/1570487241755197440).

- [Mapped Diagnostic Context in Java's Logback logging framework](https://logback.qos.ch/manual/mdc.html)

- [Handle pattern with servant to build flexible web-apps in Haskell](https://www.reddit.com/r/haskell/comments/z13xvj/tutorial_handle_pattern_with_servant_to_build/) Another "toy" project for demostrating app architecture ideas.

