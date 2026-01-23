// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  azimuth::assert_eq(100, azimuth::add(50, 50))
  azimuth::assert_eq(-100, azimuth::add(-50, -50))
  azimuth::assert_eq(0, azimuth::add(50, -50))
  azimuth::assert_eq(999999, azimuth::add(500000, 499999))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合场景
  azimuth::assert_eq(64, azimuth::multiply(8, 8))
  azimuth::assert_eq(-64, azimuth::multiply(-8, 8))
  azimuth::assert_eq(64, azimuth::multiply(-8, -8))
  azimuth::assert_eq(0, azimuth::multiply(12345, 0))
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  azimuth::assert_eq_string("Hello, Bonjour!", azimuth::greet("Bonjour"))
  azimuth::assert_eq_string("Hello, Hola!", azimuth::greet("Hola"))
  azimuth::assert_eq_string("Hello, こんにちは!", azimuth::greet("こんにちは"))
  azimuth::assert_eq_string("Hello, 안녕하세요!", azimuth::greet("안녕하세요"))
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  azimuth::assert_eq(3, azimuth::divide_with_ceil(8, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(2, 2))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base_value = 10
  let multiplier = 5
  let divisor = 12
  
  let step1 = azimuth::multiply(base_value, multiplier)  // 50
  let step2 = azimuth::add(step1, 20)                    // 70
  let result = azimuth::divide_with_ceil(step2, divisor)  // ceil(70/12) = 6
  
  azimuth::assert_eq(6, result)
}

test "mathematical_identity_verification" {
  // 数学恒等式验证
  let test_value = 42
  
  // 加法恒等式
  azimuth::assert_eq(test_value, azimuth::add(test_value, 0))
  
  // 乘法恒等式
  azimuth::assert_eq(test_value, azimuth::multiply(test_value, 1))
  azimuth::assert_eq(0, azimuth::multiply(test_value, 0))
  azimuth::assert_eq(-test_value, azimuth::multiply(test_value, -1))
}

test "business_logic_inventory" {
  // 业务逻辑：库存管理测试
  let total_items = 158
  let container_capacity = 25
  
  let containers_needed = azimuth::divide_with_ceil(total_items, container_capacity)
  azimuth::assert_eq(7, containers_needed)  // ceil(158/25) = 7
  
  let total_capacity = azimuth::multiply(containers_needed, container_capacity)
  azimuth::assert_eq(175, total_capacity)
}

test "string_input_validation" {
  // 字符串输入验证测试
  azimuth::assert_eq_string("Hello, Test123!", azimuth::greet("Test123"))
  azimuth::assert_eq_string("Hello, !@#$!", azimuth::greet("!@#$"))
  azimuth::assert_eq_string("Hello, space !", azimuth::greet("space "))
  azimuth::assert_eq_string("Hello,  space!", azimuth::greet(" space"))
}

test "edge_case_division" {
  // 边界情况除法测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "sequential_computation_chain" {
  // 连续计算链测试
  let initial = 15
  
  let step1 = azimuth::add(initial, 10)           // 25
  let step2 = azimuth::multiply(step1, 3)         // 75
  let step3 = azimuth::divide_with_ceil(step2, 20) // ceil(75/20) = 4
  let step4 = azimuth::add(step3, azimuth::multiply(2, 3))  // 4 + 6 = 10
  
  azimuth::assert_eq(10, step4)
}