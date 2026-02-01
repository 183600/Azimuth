// 标准 MoonBit 测试用例 - 新增10个高质量测试
// 覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(24, multiply(6, 4))
  assert_eq(-3, subtract(5, 8))
  assert_eq(4, divide_with_ceil(13, 4))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_values_handling" {
  // 测试边界值处理
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_business_calculation" {
  // 实际业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total)
}

test "fractional_division_precision" {
  // 测试分数除法精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "complex_nested_operations" {
  // 测试复杂嵌套操作
  let x = 10
  let y = 15
  let z = 20
  
  // 计算 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(499, result)  // (10 + 15) * 20 - ceil(10/15) = 25*20 - 1 = 500 - 1 = 499
  
  // 计算复杂的包装需求
  let items = 47
  let box_capacity = 12
  let total_boxes = divide_with_ceil(items, box_capacity)
  assert_eq(4, total_boxes)  // ceil(47/12) = 4
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
}

test "performance_efficiency_scenarios" {
  // 性能效率场景测试
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -overhead)
  
  // 计算性能提升倍数
  let performance_improvement = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(3, performance_improvement)
}