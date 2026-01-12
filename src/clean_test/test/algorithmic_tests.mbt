test "fibonacci_sequence" {
  let n0 = 0
  let n1 = 1
  let n2 = @clean_test.add(n0, n1)
  let n3 = @clean_test.add(n1, n2)
  let n4 = @clean_test.add(n2, n3)
  let n5 = @clean_test.add(n3, n4)
  
  @clean_test.assert_eq(1, n2)
  @clean_test.assert_eq(2, n3)
  @clean_test.assert_eq(3, n4)
  @clean_test.assert_eq(5, n5)
}

// 测试用例2: 阶乘计算
///|
test "factorial_calculation" {
  let fact1 = 1
  let fact2 = @clean_test.multiply(2, fact1)
  let fact3 = @clean_test.multiply(3, fact2)
  let fact4 = @clean_test.multiply(4, fact3)
  let fact5 = @clean_test.multiply(5, fact4)
  
  @clean_test.assert_eq(1, fact1)
  @clean_test.assert_eq(2, fact2)
  @clean_test.assert_eq(6, fact3)
  @clean_test.assert_eq(24, fact4)
  @clean_test.assert_eq(120, fact5)
}

// 测试用例3: 幂运算
///|
test "power_calculation" {
  let base = 2
  
  let power1 = @clean_test.multiply(base, base)
  let power2 = @clean_test.multiply(power1, base)
  let power3 = @clean_test.multiply(power2, base)
  
  @clean_test.assert_eq(4, power1)
  @clean_test.assert_eq(8, power2)
  @clean_test.assert_eq(16, power3)
}

// 测试用例4: 累加求和
///|
test "cumulative_sum" {
  let sum1 = @clean_test.add(1, 2)
  let sum2 = @clean_test.add(sum1, 3)
  let sum3 = @clean_test.add(sum2, 4)
  let sum4 = @clean_test.add(sum3, 5)
  
  @clean_test.assert_eq(3, sum1)
  @clean_test.assert_eq(6, sum2)
  @clean_test.assert_eq(10, sum3)
  @clean_test.assert_eq(15, sum4)
}

// 测试用例5: 乘法表
///|
test "multiplication_table" {
  let base = 7
  
  let m1 = @clean_test.multiply(base, 1)
  let m2 = @clean_test.multiply(base, 2)
  let m3 = @clean_test.multiply(base, 3)
  let m4 = @clean_test.multiply(base, 4)
  let m5 = @clean_test.multiply(base, 5)
  
  @clean_test.assert_eq(7, m1)
  @clean_test.assert_eq(14, m2)
  @clean_test.assert_eq(21, m3)
  @clean_test.assert_eq(28, m4)
  @clean_test.assert_eq(35, m5)
}
