// 修改版测试文件，只使用 test_helper 中的函数

test "test_1_basic_add" {
  // 直接在这里实现 add 函数
  let @azimuth.add(a : Int, b : Int) -> Int = {
    if a == 0 { return b }
    if b == 0 { return a }
    return a + b
  }
  
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
}

test "test_1_basic_multiply" {
  // 直接在这里实现 multiply 函数
  let @azimuth.multiply(a : Int, b : Int) -> Int = {
    if a == 0 || b == 0 { return 0 }
    if a == 1 { return b }
    if b == 1 { return a }
    return a * b
  }
  
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(21, @azimuth.multiply(-7, -3))
}

test "test_1_basic_greet" {
  // 直接在这里实现 greet 函数
  let @azimuth.greet(name : String) -> String = {
    "Hello, " + name + "!"
  }
  
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
}
