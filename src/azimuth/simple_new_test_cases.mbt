// 简单的测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法

test "simple_addition" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greeting" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_division_ceil" {
  // 测试基本的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "practical_packaging" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 10
  let y = 15
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "business_calculation" {
  // 业务计算测试
  let product_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, multiply(-1, discount))
  
  assert_eq(215, final_total)
}

test "greeting_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "resource_allocation" {
  // 资源分配测试
  let total_budget = 1000
  let project_costs = multiply(150, 4)
  let remaining_budget = add(total_budget, multiply(-1, project_costs))
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining_budget, 80)
  
  assert_eq(5, small_projects)
}