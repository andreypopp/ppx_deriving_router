
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
  ## POST /Route_with_implicit_path?param=ok
  Method not allowed
  ## GET /Route_with_implicit_path_post
  Method not allowed
  ## POST /Route_with_implicit_path_post
  posted
  # TESTING ROUTE MATCHING GENERATION (API)
  ## GET /
  []
  ## POST /
  {"id":42}
  ## GET /121
  {"id":121}
  ## GET /raw-response
  RAW RESPONSE
