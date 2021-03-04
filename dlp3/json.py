from collections import UserDict, UserList
import json


class JsonBase():
    def load(self):
        try:
            with self.filename.open() as f:
                self.data = json.load(f)
        except OSError:
            pass
        return self.data

    def save(self):
        with self.filename.open('w') as f:
            json.dump(self.data, f, indent=4, sort_keys=True)


class JsonDict(UserDict, JsonBase):
    def __init__(self, filename):
        self.filename = filename
        super().__init__()
        self.load()


class JsonList(UserList, JsonBase):
    def __init__(self, filename, name):
        self.filename = filename
        self.name = name
        super().__init__()
        self.load()

    def print(self):
        if not self.data:
            print(f"Currently not {self.name}ing any packages.")
        else:
            print(f"Currently {self.name}ing the following packages")
            for p in self.data:
                print(f"  {p}")

