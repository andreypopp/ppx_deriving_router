
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
  200 OK: HOME PAGE
  ## GET /hello/world
  200 OK: Hello, world!
  ## GET /hello/world?modifier=uppercase
  200 OK: Hello, WORLD!
  ## GET /Route_with_implicit_path
  200 OK: works as well, param is: -
  ## GET /Route_with_implicit_path?param=ok
  200 OK: works as well, param is: ok
  ## POST /Route_with_implicit_path?param=ok
  405 Method Not Allowed: Method not allowed
  ## GET /Route_with_implicit_path_post
  405 Method Not Allowed: Method not allowed
  ## POST /Route_with_implicit_path_post
  200 OK: posted
  ## GET /Echo_options?options={a:42}
  200 OK: {"a":42}
  ## GET /User_info?user_id=username
  200 OK: User info for "username"
  ## GET /user/username_via_path
  200 OK: User info for "username_via_path"
  ## GET /Signal?level=2
  200 OK: Signal: 2
  ## GET /List_users?user_ids=u1&user_ids=u2
  200 OK: User ids = [u1, u2]
  ## GET /List_users
  200 OK: User ids = []
  # TESTING ROUTE MATCHING GENERATION (API)
  ## GET /
  200 OK: []
  ## POST /
  400 Bad Request: Invalid or missing request body: Blank input data
  ## POST /
  400 Bad Request: Invalid or missing request body: Expected int, got object
  ## POST /
  200 OK: {"id":1}
  ## GET /121
  200 OK: {"id":121}
  ## GET /raw-response
  200 OK: RAW RESPONSE
  # TESTING ROUTE MATCHING GENERATION (ALL)
  ## GET /hello/world
  200 OK: Hello, world!
  ## GET /
  200 OK: HOME PAGE
  ## GET /nested/api/121
  200 OK: {"id":121}
  ## GET /hello/pct%20encoded?greeting=pct%20encoded
  200 OK: pct encoded, pct encoded!
  ## GET /static/js/main.js
  200 OK: path="/js/main.js"
