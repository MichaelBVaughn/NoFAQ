from flask import Flask, flash
from flask import request
from flask import render_template
from flask import jsonify
from flask.ext.bootstrap import Bootstrap
from wtforms import Form, StringField, validators, FieldList, FormField, SubmitField, HiddenField
from dataModel import *
import hashlib
import unicodedata
import wtforms.validators

MAX_LEN = 2048
app = Flask(__name__)
Bootstrap(app)

shell_ids = {"Bash" : 1, "PowerShell" : 2, "cmd" : 3, "tcsh" : 4, "zsh" : 5, "ksh" : 6 }

def hash_text(text):
    return hashlib.md5(text).hexdigest()

class ExampleForm(Form):
    shell_type = HiddenField()
    command = HiddenField()
    error = HiddenField()
    submit_fix = HiddenField()
    fixForm = HiddenField()

class ExampleBatch(Form):
    examples = FieldList(FormField(ExampleForm), min_entries = 1)
    add_one = SubmitField('Add Another Command')
    submit_all = SubmitField('Submit All')

@app.route('/licenses.html', methods=['GET','POST'])
def present_licenses():
    return render_template('/licenses.html')

@app.route('/disclaimer.html', methods=['GET','POST'])
def present_disclaimer():
    return render_template('/disclaimer.html')
    
@app.route('/submit.html', methods=['GET','POST'])
def present_page():
    form = ExampleForm(request.form)
    if request.method == 'POST' and form.validate():
        resp_dict = {'new':True, 'exampleID':42}
        #resp_dict = handle_example(form, True)            
        
        return jsonify(**resp_dict)
    return render_template('/submit.html', form=form)
            
    
def handle_form_entries(a_formlist, insert):
    for form in a_formlist:
        handle_example(form, insert)

def unicode_to_str(unicode_txt):
    return unicodedata.normalize('NFKD', unicode_txt).encode('ascii', 'ignore')

def should_submit_fix(ex_form):
    return unicode_to_str(ex_form.submit_fix.data) == "yes"

def get_shell_id(ex_form):
    shell_txt = unicode_to_str(ex_form.shell_type.data)
    #return shell_ids[shell_txt]
    return 1

def handle_example(ex_form, insert):
    #import pdb; pdb.set_trace()
    shell_type = get_shell_id(ex_form) #TO DO: Bash is one. This is hard-coded for now.    
    cmd_txt_unicode = ex_form.command.data
    cmd_txt = unicode_to_str(cmd_txt_unicode)
    cmd_txt = canonical_str(cmd_txt)
    cmd_len, cmd_fst = count_and_first_word(cmd_txt)
    err_txt_unicode = ex_form.error.data
    err_txt = unicode_to_str(err_txt_unicode)
    err_txt = canonical_str(err_txt)
    err_len, err_fst = count_and_first_word(err_txt)
    fix_txt_unicode = ex_form.fixForm.data
    fix_txt = unicode_to_str(fix_txt_unicode)
    fix_txt = canonical_str(fix_txt)
    fix_len, fix_fst = count_and_first_word(fix_txt)
    cmd_hash = hash_text(cmd_txt)
    err_hash = hash_text(err_txt)
    fix_hash = hash_text(fix_txt)
    if insert:
        cmd, cmd_created = Command.get_or_create(txt_hash = cmd_hash, first_word = cmd_fst, word_count = cmd_len, text = cmd_txt)
        err, err_created = Output.get_or_create(txt_hash = err_hash, first_word = err_fst, word_count = err_len, text = err_txt)
        inv, inv_created = Invocation.get_or_create(shell_id = shell_type, cmd_id=cmd.id, out_id=err.id)
        if should_submit_fix(ex_form):
            fix, fix_created = Fix.get_or_create(txt_hash = fix_hash, first_word = fix_fst, word_count = fix_len, text = fix_txt)
            rEx, rex_created = RepairExample.get_or_create(invocation_id=inv.id, fix_id=fix.id)
            return {'new':rex_created, 'exampleID':rEx.id}

def count_and_first_word(text):
    toks = text.split()
    return (len(toks), toks[0])
#split string into tokens then rejoin with one space
def canonical_str(text):
    toks = text.split()
    return " ".join(toks)
    
                        
import os    
if __name__=="__main__":
    app.secret_key = 'placeholder'
    port = 5000
    app.run(host="0.0.0.0", port=port, debug=True)




