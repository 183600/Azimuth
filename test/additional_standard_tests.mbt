// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

// 导入 azimuth 包中的函数
test "add_function_commutative" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  assert_eq(3, divide_with_ceil(7, 3))  // 7/3 = 2.333... -> 3
  assert_eq(2, divide_with_ceil(6, 3))  // 6/3 = 2 -> 2
  assert_eq(1, divide_with_ceil(2, 3))  // 2/3 = 0.666... -> 1
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0的情况
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(282, total_before_discount)
  assert_eq(262, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_products = 58
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(5, boxes_needed)  // ceil(58/12) = 5
}

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (7 + 9) = 4 * 7 + 4 * 9 = 64
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_per_day = 7
  let total_tasks = 50
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)  // 每天8小时
  
  assert_eq(8, days_needed)  // ceil(50/7) = 8
  assert_eq(64, total_hours)  // 8 * 8 = 64
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(0, 0))  // 0除以0
  assert_eq(0, divide_with_ceil(100, 0))  // 正数除以0
  assert_eq(0, divide_with_ceil(-100, 0))  // 负数除以0
  assert_eq(0, multiply(0, 0))  // 0乘以0
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 12345!", greet("12345"))  // 数字字符串
  assert_eq_string("Hello, Space Test!", greet("Space Test"))  // 包含空格
}