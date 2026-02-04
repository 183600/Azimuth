test "add_function_basic" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
}

test "add_function_negative" {
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
}

test "multiply_function_basic" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "multiply_function_negative" {
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-21, @azimuth.multiply(7, -3))
}

test "greet_function" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
}

test "divide_with_ceil_negative" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "divide_with_ceil_zero_division" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
}

test "divide_with_ceil_edge_cases" {
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
}

// 新增的标准测试用例
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_special_cases" {
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "complex_arithmetic_operations" {
  let sum = @azimuth.add(15, 25)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(80, product)
  
  let result = @azimuth.divide_with_ceil(product, 15)
  @azimuth.assert_eq(6, result)
}

test "mixed_operations_with_strings" {
  let num1 = 100
  let num2 = 200
  let sum = @azimuth.add(num1, num2)
  let product = @azimuth.multiply(sum, 3)
  let greeting = @azimuth.greet("User")
  
  @azimuth.assert_eq(900, product)
  @azimuth.assert_eq_string("Hello, User!", greeting)
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(sum, 150))
}

// 新增的10个标准 MoonBit 测试用例
test "azimuth_enhanced_arithmetic_operations" {
  // 增强算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-24, @azimuth.multiply(-6, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(7, 10))
}

test "azimuth_enhanced_divide_with_ceil" {
  // 增强向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "azimuth_enhanced_greet_function" {
  // 增强问候函数测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 15
  let b = 9
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律验证
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, 3))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "azimuth_business_packaging" {
  // 业务包装测试
  let total_products = 87
  let box_capacity = 20
  
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(5, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(100, total_capacity)
  
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(13, remaining_space)
}

test "azimuth_complex_calculation" {
  // 复杂计算测试
  let team_members = 8
  let tasks_per_member = 6
  let sprint_capacity = 25
  
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  @azimuth.assert_eq(48, total_tasks)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(2, sprints_needed)
  
  let total_work = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(50, total_work)
}

test "azimuth_error_handling" {
  // 错误处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "azimuth_resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  
  @azimuth.assert_eq(34, min_resources_per_project)
  
  let total_allocated = @azimuth.multiply(min_resources_per_project, projects)
  @azimuth.assert_eq(102, total_allocated)
  
  let resource_overhead = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "azimuth_performance_benchmark" {
  // 性能基准测试
  let large_number = 10000
  
  let sum_result = @azimuth.add(large_number, large_number)
  let product_result = @azimuth.multiply(large_number, 100)
  let division_result = @azimuth.divide_with_ceil(product_result, large_number)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(100, division_result)
}

test "azimuth_real_world_application" {
  // 真实世界应用测试
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  let total_budget = @azimuth.multiply(tables_needed, budget_per_table)
  @azimuth.assert_eq(450, total_budget)
  
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(6, budget_per_person)
}