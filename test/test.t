
  $ ./test.exe test
  # TESTING HREF GENERATION
  /
  /Route_with_implicit_path
  /Route_with_implicit_path?param=ok
  /hello/world
  /hello/world?modifier=uppercase
  /121
  /hello/world
  /api/121
  # TESTING ROUTE MATCHING GENERATION
  ## GET /
  OK: HOME PAGE
  ## GET /hello/world
  OK: Hello, world!
  ## GET /hello/world?modifier=uppercase
  OK: Hello, WORLD!
  ## GET /Route_with_implicit_path
  OK: works as well, param is: -
  ## GET /Route_with_implicit_path?param=ok
  OK: works as well, param is: ok
  ## POST /Route_with_implicit_path?param=ok
  Method Not Allowed: Method not allowed
  ## GET /Route_with_implicit_path_post
  Method Not Allowed: Method not allowed
  ## POST /Route_with_implicit_path_post
  OK: posted
  # TESTING ROUTE MATCHING GENERATION (API)
  ## GET /
  OK: []
  ## POST /
  Bad Request: Invalid or missing request body: Blank input data
  ## POST /
  Bad Request: Invalid or missing request body: Expected int, got object
  ## POST /
  OK: {"id":1}
  ## GET /121
  OK: {"id":121}
  ## GET /raw-response
  OK: RAW RESPONSE
  # TESTING ROUTE MATCHING GENERATION (ALL)
  ## GET /hello/world
  OK: Hello, world!
  ## GET /
  OK: HOME PAGE
  ## GET /api/121
  OK: {"id":121}
