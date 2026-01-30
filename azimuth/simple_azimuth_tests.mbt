// 简单的 Azimuth 测试用例
// 使用标准 MoonBit 测试语法

test "basic_addition" {
  add(2, 3) = 5
  add(0, 0) = 0
  add(-5, 5) = 0
}

test "basic_multiplication" {
  multiply(3, 4) = 12
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
}

test "basic_division_ceil" {
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 2) = 1
}

test "complex_calculation" {
  // 测试复杂计算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  result1 = 26  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(25, 6)
  result2 = 5  // ceil(25/6) = 5
}