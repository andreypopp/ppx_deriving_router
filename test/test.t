
  $ ./test.exe test
  # TESTING HREF GENERATION
  /
  /Route_with_implicit_path
  /Route_with_implicit_path?param=ok
  /hello/world
  /hello/world?modifier=uppercase
  # TESTING ROUTE MATCHING GENERATION
  ## GET /
  HOME PAGE
  ## GET /hello/world
  Hello, world!
  ## GET /hello/world?modifier=uppercase
  Hello, WORLD!
  ## GET /Route_with_implicit_path
  works as well, param is: -
  ## GET /Route_with_implicit_path?param=ok
  works as well, param is: ok
