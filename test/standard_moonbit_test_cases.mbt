// 标准 MoonBit 测试用例 - 覆盖核心功能的各种场景

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(3, divide_with_ceil(-12, -4))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(265, final_total)
}

test "mathematical_series_sum" {
  // 数学级数求和：1 + 2 + 3 + ... + 10
  let sum = add(add(add(add(1, 2), add(3, 4)), add(5, 6)), add(add(7, 8), add(9, 10)))
  assert_eq(55, sum)
}

test "packaging_optimization" {
  // 包装优化：计算最少需要的包装箱数量
  let total_items = 58
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(5, boxes_needed)
}

test "time_conversion" {
  // 时间转换：将分钟转换为小时和剩余分钟
  let total_minutes = 145
  let hours = divide_with_ceil(total_minutes, 60)
  let remaining_minutes = add(total_minutes, -multiply(hours - 1, 60))
  
  assert_eq(3, hours)
  assert_eq(25, remaining_minutes)
}