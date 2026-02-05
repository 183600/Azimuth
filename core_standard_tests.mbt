// 标准 MoonBit 测试用例
// 覆盖核心模块功能，包括 int、math、result 等

test "int_max_min_values" {
  // 测试 int 类型的最大值和最小值
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
}

test "int_abs_function" {
  // 测试 int 绝对值函数
  assert_eq(@int.abs(5), 5)
  assert_eq(@int.abs(-5), 5)
  assert_eq(@int.abs(0), 0)
  assert_eq(@int.abs(-2147483648), 2147483648)  // 注意：这是边界情况
}

test "math_cbrtf_function" {
  // 测试数学立方根函数
  inspect(@math.cbrtf(1), content="1")
  inspect(@math.cbrtf(8), content="2")
  inspect(@math.cbrtf(27), content="3")
  inspect(@math.cbrtf(0), content="0")
}

test "math_hypotf_function" {
  // 测试数学直角三角形斜边函数
  inspect(@math.hypotf(3, 4), content="5")
  inspect(@math.hypotf(5, 12), content="13")
  inspect(@math.hypotf(8, 15), content="17")
  inspect(@math.hypotf(0, 0), content="0")
}

test "result_ok_creation_and_pattern_matching" {
  // 测试 Result 类型的 Ok 变体创建和模式匹配
  let result : Result[Int, String] = Ok(42)
  
  let value = match result {
    Ok(v) => v
    Err(_) => -1
  }
  assert_eq(value, 42)
  
  // 测试 is_ok 方法
  assert_true(result is Ok(_))
  assert_false(result is Err(_))
}

test "result_err_creation_and_pattern_matching" {
  // 测试 Result 类型的 Err 变体创建和模式匹配
  let result : Result[Int, String] = Err("Error message")
  
  let error_message = match result {
    Ok(_) => "No error"
    Err(msg) => msg
  }
  assert_eq(error_message, "Error message")
  
  // 测试 is_err 方法
  assert_false(result is Ok(_))
  assert_true(result is Err(_))
}

test "result_map_and_map_err_functions" {
  // 测试 Result 的 map 和 map_err 函数
  let ok_result : Result[Int, String] = Ok(42)
  let mapped_ok = ok_result.map(x => x * 2)
  assert_eq(mapped_ok, Ok(84))
  
  let err_result : Result[Int, String] = Err("Error")
  let mapped_err = err_result.map_err(msg => msg + " occurred")
  assert_eq(mapped_err, Err("Error occurred"))
}

test "result_or_and_or_else_functions" {
  // 测试 Result 的 or 和 or_else 函数
  let ok_result : Result[Int, String] = Ok(42)
  let ok_value = ok_result.or(0)
  assert_eq(ok_value, 42)
  
  let err_result : Result[Int, String] = Err("Error")
  let err_value = err_result.or(0)
  assert_eq(err_value, 0)
  
  let err_value_lazy = err_result.or_else(() => -1)
  assert_eq(err_value_lazy, -1)
}

test "result_to_option_conversion" {
  // 测试 Result 到 Option 的转换
  let ok_result : Result[Int, String] = Ok(42)
  let ok_option = ok_result.to_option()
  assert_eq(ok_option, Some(42))
  
  let err_result : Result[Int, String] = Err("Error")
  let err_option = err_result.to_option()
  assert_eq(err_option, None)
}

test "result_bind_and_flatten_operations" {
  // 测试 Result 的 bind 和 flatten 操作
  let result : Result[Int, String] = Ok(42)
  let bind_result = result.bind(x => Ok(x * 2))
  assert_eq(bind_result, Ok(84))
  
  let nested_result : Result[Result[Int, String], String] = Ok(Ok(42))
  let flattened = nested_result.flatten()
  assert_eq(flattened, Ok(42))
  
  let nested_err : Result[Result[Int, String], String] = Ok(Err("inner error"))
  let flattened_err = nested_err.flatten()
  assert_eq(flattened_err, Err("inner error"))
}