// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  add(2, 3) = 5
  add(0, 0) = 0
  add(-5, 5) = 0
  add(-3, -7) = -10
  add(1000, 2000) = 3000
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况
  multiply(3, 4) = 12
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-3, -3) = 9
  multiply(100, 100) = 10000
}

test "subtract_function_validation" {
  // 验证减法函数
  subtract(10, 3) = 7
  subtract(5, 5) = 0
  subtract(3, 10) = -7
  subtract(-5, -3) = -2
  subtract(1000, 500) = 500
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 2) = 1
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(5, 0) = 0
  divide_with_ceil(-10, 3) = -3
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("café") = "Hello, café!"
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法交换律
  add(7, 13) = add(13, 7)
  
  // 乘法交换律
  multiply(7, 13) = multiply(13, 7)
  
  // 加法恒等式
  add(5, 0) = 5
  
  // 乘法恒等式
  multiply(5, 1) = 5
  multiply(5, 0) = 0
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  // 验证计算结果
  total_tasks = 40
  sprints_needed = 3
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证需要的箱子数量
  boxes_needed = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  total_capacity = 30
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  remaining_space = 5
}