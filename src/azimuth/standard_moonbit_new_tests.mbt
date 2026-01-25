// 标准 MoonBit 测试用例 - 新增10个测试
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 0.01 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))  // 0/100 = 0
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))  // 空字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))  // 单字符
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))  // 数字字符串
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列前n项和
  let first_term = 3
  let common_diff = 2
  let n_terms = 5
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(n_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n_terms - 1, common_diff))),
    2
  )
  
  @azimuth.assert_eq(35, sum)  // 3 + 5 + 7 + 9 + 11 = 35
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 1000
  let project_cost = 150
  let max_projects = @azimuth.divide_with_ceil(total_budget, project_cost)
  
  @azimuth.assert_eq(7, max_projects)  // ceil(1000/150) = 7
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(days_needed, 8)  // 每天8小时工作
  
  @azimuth.assert_eq(6, days_needed)  // ceil(35/6) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "negative_number_division" {
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))   // -10/-3 = 3.666... -> 4
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))    // -6/3 = -2 -> -2
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
  @azimuth.assert_eq_string("Hello, a@b.com!", @azimuth.greet("a@b.com"))
}