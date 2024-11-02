#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]] sexp cpp11_hello_world_() {
  writable::strings x = {"foo", "bar"};
  writable::doubles y = {0.0, 1.0};
  writable::list z = {x, y};

  return z;
}

[[cpp11::register]] bool cpp11_test_attributes_() {
  return true;
}
