// Azimuth 项目新增测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_addition" {
  // 基础算术加法测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(1000, add(500, 500))
}

test "basic_arithmetic_multiplication" {
  // 基础算术乘法测试
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 123))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-30, multiply(6, -5))
  assert_eq(10000, multiply(100, 100))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "ceil_division_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtraction_basic_operations" {
  // 基础减法运算测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  let total_products = 47
  let package_capacity = 12
  let packages_needed = divide_with_ceil(total_products, package_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(3, divide_with_ceil(-8, -3))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial_value = 20
  
  // 计算步骤：(20 * 3) / 4 + 5
  let step1 = multiply(initial_value, 3)
  let step2 = divide_with_ceil(step1, 4)
  let final_result = add(step2, 5)
  
  assert_eq(20, final_result)  // ceil(60/4) + 5 = 15 + 5 = 20
}

test "error_handling_edge_cases" {
  // 错误处理边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(101, divide_with_ceil(100, 1))
}