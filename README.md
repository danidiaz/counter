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

Links
-----

- [Servant documentation](https://docs.servant.dev/en/stable/)
- [servant on Hackage](https://hackage.haskell.org/package/servant)
- [servant-server on Hackage](https://hackage.haskell.org/package/servant-server)
- [Named routes in Servant](https://www.tweag.io/blog/2022-02-24-named-routes/)

- [How to Haskell: Sharing Data Types is Tight Coupling](https://leapyear.io/resources/blog-posts/how-to-haskell-sharing-data-types-is-tight-coupling/). [tweet](https://twitter.com/DiazCarrete/status/1570487241755197440).