// 标准 MoonBit 测试用例
// 为 azimuth 项目提供的高质量测试覆盖

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "division_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greeting_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "real_world_business_calculations" {
  // 真实业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(215, final_total)
}

test "packaging_and_distribution" {
  // 包装和分发计算
  let total_products = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  let empty_space = add(total_capacity, -total_products)
  
  assert_eq(1, empty_space)
}

test "time_and_resource_planning" {
  // 时间和资源规划
  let tasks_per_day = 6
  let total_tasks = 25
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(5, days_needed)
  
  // 计算总工作小时（每天8小时）
  let total_hours = multiply(days_needed, 8)
  assert_eq(40, total_hours)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端值
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(2147483647, add(2147483647, 0))
}

test "string_processing_internationalization" {
  // 字符串处理和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 复杂表达式：(base * multiplier) / divisor + offset
  let intermediate = multiply(base_value, multiplier)
  let division_result = divide_with_ceil(intermediate, divisor)
  let final_result = add(division_result, offset)
  
  assert_eq(15, final_result)  // ceil(10*3/4) + 7 = ceil(30/4) + 7 = 8 + 7 = 15
}