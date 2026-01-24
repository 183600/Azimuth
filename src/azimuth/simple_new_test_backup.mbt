// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的主要功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation_scenario" {
  // 业务计算场景：计算订单总价
  let unit_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(110, total)
}

test "packaging_efficiency_test" {
  // 包装效率测试：计算需要的箱子数量
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中国!", greet("中国"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流
  let base_value = 100
  let multiplier = 3
  let addition = 25
  
  let step1 = multiply(base_value, multiplier)
  let result = add(step1, addition)
  
  assert_eq(325, result)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

// 新增的测试用例
test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100, divide_with_ceil(9999, 100))
}

test "unicode_greeting" {
  // 测试 Unicode 字符问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "complex_financial_calculation" {
  // 复杂金融计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "resource_allocation" {
  // 资源分配计算
  let total_budget = 10000
  let project_costs = add(add(3500, 2800), 2200)
  let remaining = add(total_budget, -project_costs)
  let additional_projects = divide_with_ceil(remaining, 600)
  
  assert_eq(8500, project_costs)
  assert_eq(1500, remaining)
  assert_eq(3, additional_projects)
}