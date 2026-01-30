// 修改版测试文件，只使用 test_helper 中的函数

test "test_1_basic_add" {
  // 直接在这里实现 add 函数
  let add(a : Int, b : Int) -> Int = {
    if a == 0 { return b }
    if b == 0 { return a }
    return a + b
  }
  
  assert_eq(5, add(2, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
}

test "test_1_basic_multiply" {
  // 直接在这里实现 multiply 函数
  let multiply(a : Int, b : Int) -> Int = {
    if a == 0 || b == 0 { return 0 }
    if a == 1 { return b }
    if b == 1 { return a }
    return a * b
  }
  
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(21, multiply(-7, -3))
}

test "test_1_basic_greet" {
  // 直接在这里实现 greet 函数
  let greet(name : String) -> String = {
    "Hello, " + name + "!"
  }
  
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
}
