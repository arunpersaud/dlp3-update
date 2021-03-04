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


class JsonList(UserList, JsonBase):
    def __init__(self, filename):
        self.filename = filename
        super().__init__()

