// 单个测试文件
test "single_test" {
  assert_eq(5, add(2, 3))
}

test "add_function_basic" {
  // 基本加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_commutative" {
  // 乘法交换律测试
  assert_eq(21, multiply(7, 3))
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
}

test "greet_function_standard" {
  // 问候函数标准测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_zero_divisor" {
  // 除零测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "subtract_function_basic" {
  // 基本减法测试
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(5, 5))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
}

// 新增的10个标准 MoonBit 测试用例
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties_extended" {
  // 测试数学性质扩展
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "greeting_functionality_extended" {
  // 测试问候功能扩展
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, User123!", greet("User123"))
}

test "divide_with_ceil_edge_cases_extended" {
  // 测试向上取整除法的边界情况扩展
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "negative_number_operations_extended" {
  // 测试负数运算扩展
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_shopping_cart" {
  // 模拟购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
}

test "packaging_calculation_extended" {
  // 包装计算场景扩展
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "complex_calculations_extended" {
  // 复杂计算测试扩展
  let result = add(
    multiply(5, divide_with_ceil(25, 4)),
    multiply(3, divide_with_ceil(17, 5))
  )
  assert_eq(43, result)  // 5*7 + 3*4 = 35 + 12 = 47
}

test "large_number_operations_extended" {
  // 大数运算测试扩展
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "business_order_calculation_extended" {
  // 业务订单计算扩展
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

// 用户自定义的标准 MoonBit 测试用例
test "user_custom_test_1" {
  // 基本加法测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "user_custom_test_2" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "user_custom_test_3" {
  // 向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "user_custom_test_4" {
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "user_custom_test_5" {
  // 问候函数测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "user_custom_test_6" {
  // 减法函数测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "user_custom_test_7" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "user_custom_test_8" {
  // 业务场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
}

test "user_custom_test_9" {
  // 复杂工作流测试
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
}

test "user_custom_test_10" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候
  assert_eq_string("Hello,   !", greet("  "))
}