// Azimuth 项目简洁测试用例
// 使用标准 MoonBit 测试语法编写

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let initial = 10
  let step1 = add(initial, multiply(2, 3))    // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)      // ceil(16/5) = 4
  let step3 = multiply(step2, add(1, 1))      // 4 * (1+1) = 8
  let step4 = divide_with_ceil(step3, 3)      // ceil(8/3) = 3
  
  assert_eq(3, step4)
}

test "error_handling_boundary" {
  // 错误处理和边界测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "performance_large_numbers" {
  // 大数性能测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(1000000, 1000))
  assert_eq(100001, divide_with_ceil(200000000, 2000))
  assert_eq(500001, divide_with_ceil(1000001000, 2000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}