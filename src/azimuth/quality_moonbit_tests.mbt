// 高质量 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含数学运算、字符串处理和实际应用场景的测试

// 导入 azimuth 库的函数
use azimuth::{add, multiply, greet, assert_eq, assert_eq_string, divide_with_ceil}

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  assert_eq(15, multiply(15, 1))
  assert_eq(-8, multiply(-8, 1))
  assert_eq(0, multiply(0, 1))
  assert_eq(1000, multiply(1000, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(0, divide_with_ceil(0, 5))  // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))  // 除数为零的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(400, total)
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (7 + 9) = 5 * 7 + 5 * 9 = 80
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "real_world_packaging_scenario" {
  // 实际包装场景：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "string_concatenation_edge_cases" {
  // 字符串连接边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 123!", greet("123"))  // 数字字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
}

test "performance_calculation_scenario" {
  // 性能计算场景：算法复杂度分析
  let base_operations = 1000
  let growth_factor = 3
  let overhead_percentage = 15
  
  // 计算增长后的操作数
  let grown_operations = multiply(base_operations, growth_factor)
  // 计算开销
  let overhead = divide_with_ceil(multiply(grown_operations, overhead_percentage), 100)
  // 计算实际性能
  let actual_performance = add(grown_operations, -overhead)
  
  assert_eq(3000, grown_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
}