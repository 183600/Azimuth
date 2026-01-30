// 简化版 Azimuth 标准测试用例
// 包含10个基本的高质量测试

test "basic_addition" {
  add(2, 3) = 5
  add(0, 0) = 0
  add(-5, 5) = 0
  add(100, 200) = 300
}

test "basic_multiplication" {
  multiply(3, 4) = 12
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-3, -3) = 9
}

test "basic_divide_with_ceil" {
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(5, 0) = 0
}

test "basic_greeting" {
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("世界") = "Hello, 世界!"
}

test "negative_operations" {
  add(-5, -3) = -8
  multiply(-5, 3) = -15
  multiply(-5, -3) = 15
  divide_with_ceil(-10, 3) = -3
}

test "large_numbers" {
  add(1000, 2000) = 3000
  multiply(100, 100) = 10000
  divide_with_ceil(10000, 3) = 3334
  divide_with_ceil(9999, 10000) = 1
}

test "mathematical_properties" {
  // 加法交换律
  add(7, 13) = add(13, 7)
  
  // 乘法交换律
  multiply(7, 13) = multiply(13, 7)
  
  // 零元素性质
  add(5, 0) = 5
  multiply(5, 0) = 0
}

test "business_calculation_packaging" {
  // 包装计算：25个物品，每箱6个，需要多少箱
  let total_items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  boxes_needed = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  total_capacity = 30
}

test "complex_calculation_sequence" {
  // 复杂计算序列
  let initial = 10
  let step1 = add(initial, 5)  // 15
  step1 = 15
  let step2 = multiply(step1, 2)  // 30
  step2 = 30
  let step3 = divide_with_ceil(step2, 4)  // 8
  step3 = 8
  let step4 = add(step3, 1)  // 9
  step4 = 9
}

test "unicode_and_special_characters" {
  greet("🚀") = "Hello, 🚀!"
  greet("café") = "Hello, café!"
  greet("Москва") = "Hello, Москва!"
  greet("@#$%") = "Hello, @#$%!"
}