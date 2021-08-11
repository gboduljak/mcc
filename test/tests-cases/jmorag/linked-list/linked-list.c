struct List
{
  int val;
  struct List *next;
};

struct List *nil;

struct List *cons(int x, struct List *tail) {
  struct List *head;
  head = (struct List *)malloc(sizeof(struct List));
  head->val = x;
  head->next = tail;
  return head;
}

// This one is hard to write even with a real compiler...
struct List *reverse(struct List *list) {
  struct List *this;
  struct List *next;
  struct List *new_next;
  this = list;
  next = this->next;
  this->next = nil;
  while (next != nil) {
  
    new_next = next->next;
    next->next = this;
    this = next;
    next = new_next;
  }
  return this;
}

int main() {
  int i;
  struct List *l;
  struct List *l2;
  l = nil;
  l2 = nil;
  for (i = 0; i < 20; i = i + 1) {
    l = cons(i, l);
  }
  l2 = l;
  while (l2 != nil) {
    printf("%d\n", l2->val);
    l2 = l2->next;
  }
  l2 = reverse(l);
  while (l2 != nil) {
    printf("%d\n", l2->val);
    l2 = l2->next;
  }
  return 0;
}