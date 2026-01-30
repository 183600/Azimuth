// Azimuth 核心功能测试
// 包含 8 个高质量测试用例，覆盖主要功能

test "add_basic_functionality" {
  // 测试基本加法功能
  add(2, 3) = 5
  add(0, 0) = 0
  add(-2, 1) = -1
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  multiply(2, 3) = 6
  multiply(5, 0) = 0
  multiply(-2, 2) = -4
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(5, 0) = 0
}

test "negative_number_operations" {
  // 测试负数运算
  add(-2, -3) = -5
  multiply(-2, -3) = 6
  multiply(-2, 3) = -6
  divide_with_ceil(-10, 3) = -3
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  add(x, y) = add(y, x)
  
  // 乘法交换律
  multiply(x, y) = multiply(y, x)
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  boxes_needed = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  total_capacity = 30
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let base = 10
  let step1 = add(base, 5)        // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let result = add(step3, 1)  // 5
  
  result = 5
}