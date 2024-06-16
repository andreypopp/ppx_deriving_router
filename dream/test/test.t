
  $ ./test.exe test
  # TESTING HREF GENERATION
  /
  /Route_with_implicit_path
  /Route_with_implicit_path?param=ok
  /hello/world
  /hello/world?modifier=uppercase
  /121
  /hello/world
  /nested/api/121
  /User_info?user_id=username
  /user/username
  /Signal?level=1
  /List_users?user_ids=u1&user_ids=u2
  /List_users
  /static/js/main.js
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
  ## GET /Echo_options?options={a:42}
  OK: {"a":42}
  ## GET /User_info?user_id=username
  OK: User info for "username"
  ## GET /user/username_via_path
  OK: User info for "username_via_path"
  ## GET /Signal?level=2
  OK: Signal: 2
  ## GET /List_users?user_ids=u1&user_ids=u2
  OK: User ids = [u1, u2]
  ## GET /List_users
  OK: User ids = []
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
  ## GET /nested/api/121
  OK: {"id":121}
  ## GET /hello/pct%20encoded?greeting=pct%20encoded
  OK: pct encoded, pct encoded!
  ## GET /static/js/main.js
  OK: path="/js/main.js"
