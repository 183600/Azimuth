// 标准 MoonBit 核心功能测试用例
// 测试 azimuth 库的核心功能，使用标准 MoonBit 测试语法

test "addition_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质
  assert_eq(12, multiply(12, 1))
  assert_eq(-8, multiply(-8, 1))
  assert_eq(0, multiply(0, 1))
  assert_eq(7, multiply(7, 1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-2, divide_with_ceil(-8, 4))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(999999, 1))
}

test "practical_business_calculations" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "string_processing_with_unicode" {
  // 测试 Unicode 字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "algorithm_efficiency_simulation" {
  // 模拟算法效率计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead = 150
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let actual_performance = add(optimized_ops, -overhead)
  
  assert_eq(3000, optimized_ops)
  assert_eq(2850, actual_performance)
}