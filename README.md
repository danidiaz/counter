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

Log messages look like this. Notice they include the [names](https://www.tweag.io/blog/2022-02-24-named-routes/) of the Servant handlers, as well as the component which emitted the message:

    endpoint=[(CounterCollectionAPI,"create")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"create")] component=Repository - Extra log message added by instrumentation
    endpoint=[(CounterCollectionAPI,"create")] component=Repository - withResource
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] component=Repository - withExistingResource
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] component=Repository - Extra log message added by instrumentation
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"increase")] component=Repository - withResource
    endpoint=[] component=Repository - Cleaning stale entries...
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"query")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"query")] component=Repository - withExistingResource
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"query")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"query")] component=Repository - Extra log message added by instrumentation
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"query")] component=Repository - withResource
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=DeleteCounter - Requesting counter deletion CounterId dfabbdc2-8714-42f7-a22e-1dc9ed840ed9
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=Repository - withExistingResource
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=Repository - Applies to all methods.
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=Repository - Extra log message added by instrumentation
    endpoint=[(CounterCollectionAPI,"counters"),(CounterAPI,"delete")] component=Repository - withResource

Counters that haven't been updated in a period of time (specified in the
configuration file) are deleted. This is just an excuse for playing with
asynchronous tasks.

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
    - Conversions between the API and the model should be able to have effects.
    Think for example of a conversion function which accesses a database.

- Servant handlers should be "built" using dependency injection.
    - Convenient way of wiring dependencies like loggers.
    - The dependency injection context is a good place to add debug traces.
        - It sits on top on the module hierarchy, close to `Main`, so recompilations shouldn't be too painful.
        - It's a place where all types are known and concrete, so their `Show`/`ToJSON` instances are available.
    - Only *direct* dependencies should appear in a component's signature.
        - So `Foo` depends on `Bar`, and `Bar` uses some `BazCache` internally? That's great. But `Foo` should not know *at all* about `BazCache`.
            - Adding `BazCache` to `Bar` and then having to update the signatures of a zillion clients of `Bar`, and of client's clients, is not my idea of fun.
    - I'm using my own library [dep-t](https://hackage.haskell.org/package/dep-t).

- For each log message emitted *by the model*, I want to print the Servant handler (the handler field name) resposible for triggering it.
    - BUT! Components in the model *should know nothing about handlers, or Servant*. 
        - Only the logger component is allowed to know that handlers exist.

- I aslo want to tag each log message with the component that emitted it.
Preferably, this should be done automatically.

- Each component should be in charge of parsing its *own* piece of the
configuration. When wiring components together, the different configuration
parsers should be aggregated.

- Components should be able to "register" asynchronous background tasks.

Registering the background tasks should be done when adding the corresponding
component to the dependency injection context, and in the same region of code. 
This minimizes the risk of forgetting to register tasks elsewhere.

- Provide "control" HTTP endpoints for important components, that allow admins
to inspect and set their configurations at runtime.

In the spirit of Spring Boot's "actuator" framework.

Hopefully this should be achieved witout a lot of boilerplate.

Links
-----

- [Servant documentation](https://docs.servant.dev/en/stable/)
- [servant on Hackage](https://hackage.haskell.org/package/servant)
- [servant-server on Hackage](https://hackage.haskell.org/package/servant-server)
- [Named routes in Servant](https://www.tweag.io/blog/2022-02-24-named-routes/)

- [How to Haskell: Sharing Data Types is Tight Coupling](https://leapyear.io/resources/blog-posts/how-to-haskell-sharing-data-types-is-tight-coupling/). [tweet](https://twitter.com/DiazCarrete/status/1570487241755197440).

- [Mapped Diagnostic Context in Java's Logback logging framework](https://logback.qos.ch/manual/mdc.html)

- [Handle pattern with servant to build flexible web-apps in Haskell](https://www.reddit.com/r/haskell/comments/z13xvj/tutorial_handle_pattern_with_servant_to_build/) Another "toy" project for demostrating app architecture ideas.

