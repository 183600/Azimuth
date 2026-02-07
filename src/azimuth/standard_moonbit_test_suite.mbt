// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖核心功能和各种边界情况

test "add_function_basic_operations" {
  // 加法函数基本操作测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-3, 4))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_edge_cases" {
  // 减法函数边界情况测试
  assert_eq(5, subtract(15, 10))
  assert_eq(-5, subtract(10, 15))
  assert_eq(0, subtract(20, 20))
  assert_eq(30, subtract(10, -20))
  assert_eq(-30, subtract(-10, 20))
}

test "divide_with_ceil_function_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_handling" {
  // 向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(10, -4))
  assert_eq(3, divide_with_ceil(-10, -4))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_commutativity_laws" {
  // 数学交换律验证测试
  let x = 15
  let y = 25
  
  // 加法交换律验证
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律验证
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 具体数值验证
  assert_eq(40, add(15, 25))
  assert_eq(375, multiply(15, 25))
}

test "business_calculation_scenario" {
  // 商业计算场景测试
  let unit_price = 25
  let quantity = 8
  let discount = 15
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣后价格
  let discounted_price = subtract(subtotal, discount)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  
  // 计算总价
  let total_price = add(discounted_price, tax_amount)
  
  assert_eq(200, subtotal)
  assert_eq(185, discounted_price)
  assert_eq(19, tax_amount)
  assert_eq(204, total_price)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let project_a_needs = 30
  let project_b_needs = 25
  let project_c_needs = 20
  
  // 计算已分配资源
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的新项目数量（每个项目需要12个资源）
  let new_projects = divide_with_ceil(remaining, 12)
  
  // 计算资源利用率
  let utilization_rate = divide_with_ceil(multiply(allocated, 100), total_resources)
  
  assert_eq(75, allocated)
  assert_eq(25, remaining)
  assert_eq(3, new_projects)
  assert_eq(75, utilization_rate)
}

test "comprehensive_boundary_testing" {
  // 综合边界测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, subtract(1, 0))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 大数测试
  let large_number = 100000
  assert_eq(200000, add(large_number, large_number))
  assert_eq(33334, divide_with_ceil(large_number, 3))
  
  // 特殊字符串测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}