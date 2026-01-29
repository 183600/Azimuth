// 新增的标准 MoonBit 测试用例
// 覆盖核心功能和边界情况，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法函数
  add(10, 5) = 15
  add(0, 0) = 0
  add(-8, 3) = -5
  add(1000, 2000) = 3000
  add(-10, -20) = -30
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  multiply(6, 7) = 42
  multiply(0, 123) = 0
  multiply(1, 456) = 456
  multiply(-3, 4) = -12
  multiply(-5, -5) = 25
}

test "divide_with_ceil_scenarios" {
  // 测试向上取整除法的各种场景
  divide_with_ceil(15, 4) = 4
  divide_with_ceil(16, 4) = 4
  divide_with_ceil(1, 5) = 1
  divide_with_ceil(0, 10) = 0
  divide_with_ceil(20, 0) = 0
  divide_with_ceil(-7, 3) = -2
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  greet("MoonBit") = "Hello, MoonBit!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("") = "Hello, !"
  greet("café") = "Hello, café!"
}

test "mathematical_properties" {
  // 验证数学性质
  add(8, 12) = add(12, 8)  // 加法交换律
  multiply(7, 9) = multiply(9, 7)  // 乘法交换律
  add(15, 0) = 15  // 加法单位元
  multiply(15, 1) = 15  // 乘法单位元
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)  // 40
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)  // 3
  
  sprints_needed = 3
}

test "error_handling_boundary" {
  // 错误处理和边界测试
  divide_with_ceil(0, 0) = 0
  divide_with_ceil(10, 0) = 0
  divide_with_ceil(-10, 0) = 0
  subtract(10, 10) = 0
  subtract(5, 10) = -5
}

test "resource_allocation_problem" {
  // 资源分配问题测试
  // 场景：仓库存储优化
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)  // 4
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)  // 48
  let remaining_space = subtract(total_capacity, items)  // 1
  
  remaining_space = 1
}

test "sequential_operations" {
  // 连续操作测试
  let initial = 10
  let step1 = add(initial, 5)  // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let result = add(step3, 1)  // 5
  
  result = 5
}

test "unicode_string_processing" {
  // Unicode字符串处理测试
  greet("北京") = "Hello, 北京!"
  greet("São Paulo") = "Hello, São Paulo!"
  greet("🌟") = "Hello, 🌟!"
  greet("mañana") = "Hello, mañana!"
  greet("Москва") = "Hello, Москва!"
}