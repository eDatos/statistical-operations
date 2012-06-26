package org.siemac.metamac.statistical.operations.rest.internal;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.siemac.metamac.rest.common.v1_0.domain.Error;

/**
 * JAX-RS has a RuntimeException class, called WebApplicationException, that allows you to abort your JAX-RS service method.
 * Can take an HTTP status code or even a Response object as one of its constructor parameters
 * 
 * TODO pasar a librería común
 */
public class RestException extends WebApplicationException {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Error             error;
    private Status            status;

    public RestException(Error error, Status status) {
        super(Response.status(status).entity(error).build());
    }

    public Error getError() {
        return error;
    }

    public Status getStatus() {
        return status;
    }
}
