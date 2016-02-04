#ifndef GUARD_BELIEFBASE_H
#define GUARD_BELIEFBASE_H

#include <string>
#include <vector>

class BeliefBase
{
  friend bool isSubset(const BeliefBase&, const BeliefBase&);
  
 public:
  
  BeliefBase();
  BeliefBase(const std::vector<int>&);

  std::vector<int> getItems() const;

  virtual int width() const;
  
 protected:
   int numItems_;
   std::vector<int> items_;
   
 private:
   virtual bool contains(const BeliefBase&) const;
};


#endif //GUARD_BELIEFBASE_H
