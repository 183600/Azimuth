// 使用 inspect 语句的测试演示
test "inspect_add_test" {
  inspect(azimuth::add(2, 3)) |> expect_eq(5)
}

test "inspect_multiply_test" {
  inspect(azimuth::multiply(2, 3)) |> expect_eq(6)
}

test "inspect_greet_test" {
  inspect(azimuth::greet("World")) |> expect_eq("Hello, World!")
}

test "inspect_divide_test" {
  inspect(azimuth::divide_with_ceil(10, 3)) |> expect_eq(4)
}