// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法编写

test "basic_addition_boundary" {
  // 测试基本加法边界情况
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(0, 0))
  assert_eq(-1000, add(-500, -500))
}

test "basic_multiplication_boundary" {
  // 测试基本乘法边界情况
  assert_eq(10000, multiply(100, 100))
  assert_eq(0, multiply(0, 1000))
  assert_eq(-10000, multiply(-100, 100))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_special_cases" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算
  let product_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation" {
  // 包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "mathematical_properties" {
  // 数学性质测试
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

test "resource_allocation" {
  // 资源分配测试
  let total_budget = 1000
  let project_costs = multiply(150, 4)
  let remaining_budget = add(total_budget, -project_costs)
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining_budget, 80)
  
  assert_eq(600, project_costs)
  assert_eq(400, remaining_budget)
  assert_eq(5, small_projects)
}