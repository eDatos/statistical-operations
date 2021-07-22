package org.siemac.metamac.statistical.operations.core.serviceimpl.result;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;

public abstract class Result<T> {
    T content;
    List<MetamacException> exceptions;

    protected Result() {
        exceptions = new ArrayList<>();
    }

    protected Result(T content, List<MetamacException> exceptions) {
        this.content = content;
        if (exceptions != null) {
            this.exceptions = exceptions;
        } else {
            this.exceptions = new ArrayList<>();
        }
    }

    public T getContent() {
        return content;
    }

    public void setContent(T content) {
        this.content = content;
    }

    public List<MetamacException> getExceptions() {
        return exceptions;
    }

    public void setExceptions(List<MetamacException> exceptions) {
        this.exceptions = exceptions;
    }

    public void addException(MetamacException exception) {
        exceptions.add(exception);
    }

    public boolean isOk() {
        return exceptions.isEmpty();
    }
}
