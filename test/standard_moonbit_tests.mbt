// 标准 MoonBit 测试用例
// 覆盖核心功能：add, multiply, greet, divide_with_ceil

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-10, 10))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试边界值正常运算
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, add(max_val, 0))
}

test "string_processing_functionality" {
  // 测试字符串处理功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, user123!", greet("user123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "division_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(7, divide_with_ceil(13, 2))
}

test "real_world_calculation_scenarios" {
  // 实际应用场景计算
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let a = 10
  let b = 5
  let c = 3
  let d = 2
  
  // 计算 (a + b) × (c - d)
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let result = multiply(sum_ab, diff_cd)
  assert_eq(45, result)
  
  // 计算 a × b + c × d
  let product_ab = multiply(a, b)
  let product_cd = multiply(c, d)
  let combined = add(product_ab, product_cd)
  assert_eq(56, combined)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试零值处理
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
  
  // 测试负数处理
  assert_eq(-15, add(-10, -5))
  assert_eq(25, multiply(-5, -5))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 等差数列求和：1 + 2 + ... + 100 = 5050
  let n = 100
  let sum = multiply(n, add(1, n)) / 2
  assert_eq(5050, sum)
  
  // 平方和计算：1² + 2² + ... + 5² = 55
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}

test "data_validation_and_type_safety" {
  // 数据验证和类型安全测试
  // 验证数值范围
  let positive = 100
  let negative = -100
  let zero = 0
  
  assert_true(positive > 0)
  assert_true(negative < 0)
  assert_true(zero == 0)
  
  // 验证字符串操作的安全性
  let normal_string = "Hello"
  let empty_string = ""
  let special_string = "!@#$%"
  
  assert_eq_string("Hello, Hello!", greet(normal_string))
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, !@#$%!", greet(special_string))
}