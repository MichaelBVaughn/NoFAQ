from peewee import *
from playhouse.db_url import connect

mysql_db = MySQLDatabase("db-name", host='hostname', port=80, user='user', passwd='password')
MAX_VARCHAR = 21845
class BaseModel(Model):
    class Meta:       
        database = mysql_db

class Shell(BaseModel):
    name = CharField(index = True)
        
class Command(BaseModel):
    txt_hash = CharField(index = True)
    first_word = CharField(index = True)
    word_count = IntegerField()
    text = TextField()

class Output(BaseModel):
    txt_hash = CharField(index = True)
    first_word = CharField(index = True)
    word_count = IntegerField()
    text = TextField()

class Fix(BaseModel):
    txt_hash = CharField(index = True)
    first_word = CharField(index = True)
    word_count = IntegerField()
    text = TextField()

class Session(BaseModel):
    session_id = UUIDField(primary_key = True)
    first_cmd_id = IntegerField()

class RepairExample(BaseModel):
    invocation_id = IntegerField(index = True)
    fix_id = IntegerField(index = True)        

class SessionCmd(BaseModel):
    session_id = UUIDField(index = True)
    timestamp = DateTimeField()
    next_cmd_id = IntegerField(index = True)
    invocation_id = IntegerField(index = True)

class Invocation(BaseModel):
    shell_id = IntegerField(index = True)
    cmd_id = IntegerField(index = True)        
    out_id = IntegerField(index = True)

class FixRule(BaseModel):
    cmd_name = CharField(index = True)
    cmd_len = IntegerField(index = True)
    out_len = IntegerField(index = True)
    fix_len = IntegerField(index = True)
    fix_prog = BlobField()
    
class ExampleSet(BaseModel):
    rule_id = IntegerField(index = True)
    cmd_id = IntegerField(index = True)
    

class ExampleQueue(BaseModel):
    exampleID = IntegerField(index = True)
