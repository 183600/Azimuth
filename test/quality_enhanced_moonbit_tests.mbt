// 高质量 MoonBit 测试用例 - 针对 azimuth 库的核心功能进行全面测试
// 包含边界情况、数学性质验证和实际应用场景

test "import_test" {
  // 简单的导入测试，确保函数可以正常调用
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "enhanced_mathematical_properties" {
  // 测试基本数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "enhanced_division_precision" {
  // 测试向上取整除法的精度和边界情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))   // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "enhanced_string_processing" {
  // 测试字符串处理和 Unicode 支持
  // 基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // Unicode 和表情符号支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, 👨‍👩‍👧‍👦!", greet("👨‍👩‍👧‍👦"))
  
  // 特殊字符和数字组合
  assert_eq_string("Hello, Test123!", greet("Test123"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "enhanced_business_calculations" {
  // 测试实际业务计算场景
  // 订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, add(tax, shipping))
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "enhanced_inventory_management" {
  // 测试库存管理场景
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  // 计算需要的箱子数量
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  assert_eq(5, boxes_small)    // ceil(100/24) = 5
  assert_eq(11, boxes_medium)  // ceil(250/24) = 11
  assert_eq(42, boxes_large)   // ceil(1000/24) = 42
  assert_eq(58, total_boxes)   // 5 + 11 + 42 = 58
}

test "enhanced_financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate_percent = 5
  
  // 计算复利（简化版本，使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  // 计算总收益
  let total_return = add(year3, -principal)
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
  assert_eq(1576, total_return)
}

test "enhanced_performance_simulation" {
  // 测试性能模拟计算
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

test "enhanced_extreme_boundary_values" {
  // 测试极端边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界值的运算
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483647, add(min_int, 1))
  assert_eq(0, add(max_int, add(min_int, 1)))
  
  // 测试大数除法的精度
  assert_eq(1, divide_with_ceil(1, max_int))
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(2, divide_with_ceil(add(max_int, 1), max_int))
  
  // 测试零的各种运算
  assert_eq(0, multiply(0, max_int))
  assert_eq(0, multiply(0, min_int))
  assert_eq(0, divide_with_ceil(0, max_int))
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
}