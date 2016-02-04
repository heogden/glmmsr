#ifndef GUARD_COVERSBELIEF_H
#define GUARD_COVERSBELIEF_H

#include <algorithm>

template<typename T>
class CoversBelief
{
 public:
  CoversBelief(const T&);
  bool operator()(const T&);
  
 private:
  T belief_;
};

template <typename T>
CoversBelief<T>::CoversBelief(const T& belief): belief_(belief) {}

template <typename T>
bool CoversBelief<T>::operator()(const T& other)
{
  auto items = belief_.getItems();
  auto otherItems = other.getItems();
   for(auto i = items.begin(); i != items.end(); ++i)
     if(std::find(otherItems.begin(), otherItems.end(), *i) == otherItems.end())
      return false;
  return true;
}

#endif // GUARD_COVERSBELIEF_H
