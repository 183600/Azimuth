// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "simple_addition" {
  // 测试简单加法
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  // 测试简单乘法
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  // 测试简单问候
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_test" {
  // 测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算总价
  let total = add(subtotal, shipping)
  
  assert_eq(270, subtotal)
  assert_eq(285, total)
}

test "packaging_calculation" {
  // 测试包装计算
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(4, boxes_needed)
}

test "time_calculation" {
  // 测试时间计算
  let tasks_per_day = 7
  let total_tasks = 50
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(8, days_needed)
  assert_eq(64, total_hours)
}

test "unicode_support" {
  // 测试 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
}