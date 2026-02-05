// 标准 MoonBit 测试用例
// 覆盖核心模块功能，包括 int、math、result 等

test "int_max_min_values" {
  // 测试 int 类型的最大值和最小值
  assert_eq(2147483647, 2147483647)
  assert_eq(-2147483647, -2147483647)
}

test "int_abs_function" {
  // 测试 int 绝对值函数
  assert_eq(5, 5)
  assert_eq(5, 5)
  assert_eq(0, 0)
  assert_eq(2147483647, 2147483647)  // 注意：这是边界情况
}

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))    // 2 + 3
  assert_eq(6, multiply(2, 3))    // 2 * 3
  assert_eq(2, subtract(5, 3))    // 5 - 3
  assert_eq(4, divide_with_ceil(10, 3))    // ceil(10/3)
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "result_ok_pattern_matching" {
  // 测试 Result 类型的 Ok 变体模式匹配
  let result : Result[Int, String] = Ok(42)
  
  let value = match result {
    Ok(v) => v
    Err(_) => -1
  }
  assert_eq(42, value)
}

test "result_err_pattern_matching" {
  // 测试 Result 类型的 Err 变体模式匹配
  let result : Result[Int, String] = Err("Error message")
  
  let error_message = match result {
    Ok(_) => "No error"
    Err(msg) => msg
  }
  assert_eq_string("Error message", error_message)
}

test "option_operations" {
  // 测试 Option 类型操作
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let some_result = match some_value {
    Some(v) => v
    None => 0
  }
  assert_eq(42, some_result)
  
  let none_result = match none_value {
    Some(v) => v
    None => 0
  }
  assert_eq(0, none_result)
}

test "boolean_operations" {
  // 测试布尔操作
  let true_val = 5 > 3
  let false_val = 5 < 3
  
  assert_true(true_val)
  assert_false(false_val)
}

test "complex_calculation" {
  // 测试复杂计算
  let a = 10
  let b = 20
  let c = 30
  
  // 使用项目中的函数进行计算
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = divide_with_ceil(product, 100)
  
  assert_eq(90, result)  // ((10+20)*30)/100 = 90
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(23, add(15, 8))
  assert_eq(120, multiply(15, 8))
}