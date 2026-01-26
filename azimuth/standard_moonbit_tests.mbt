// 标准 MoonBit 测试用例 - 覆盖 azimuth 库的核心功能
// 共8个测试用例，使用标准的 MoonBit 测试语法

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  add(10, 5) = 15
  add(0, 0) = 0
  add(-3, 7) = 4
  add(-5, -5) = -10
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  multiply(3, 4) = 12
  multiply(0, 100) = 0
  multiply(-2, 5) = -10
  multiply(-3, -3) = 9
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  greet("MoonBit") = "Hello, MoonBit!"
  greet("世界") = "Hello, 世界!"
  greet("café") = "Hello, café!"
  greet("") = "Hello, !"
}

test "divide_with_ceil_practical" {
  // 测试向上取整除法的实际应用场景
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(15, 5) = 3
  divide_with_ceil(7, 2) = 4
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(5, 0) = 0
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 8
  let y = 12
  
  // 加法交换律
  add(x, y) = add(y, x)
  
  // 乘法交换律
  multiply(x, y) = multiply(y, x)
  
  // 分配律
  multiply(x, add(y, 2)) = add(multiply(x, y), multiply(x, 2))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  boxes_needed = 4
  
  // 验证总容量
  multiply(boxes_needed, capacity_per_box) = 48
  
  // 验证剩余空间
  multiply(boxes_needed, capacity_per_box) - items = 1
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线
  let base = 5
  let step1 = add(base, multiply(2, 3))
  let step2 = divide_with_ceil(step1, 4)
  let result = multiply(step2, add(1, 2))
  
  result = 9
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  resources_per_team = 34
  
  // 验证总分配
  multiply(resources_per_team, teams) = 102
  
  // 验证资源利用率
  divide_with_ceil(multiply(total_resources, 100), multiply(resources_per_team, teams)) = 98
}