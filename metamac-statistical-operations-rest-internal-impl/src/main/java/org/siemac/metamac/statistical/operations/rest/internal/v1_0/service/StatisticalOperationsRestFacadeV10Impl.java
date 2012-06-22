package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.net.URI;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.Do2RestInternalMapperV10;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("statisticalOperationsRestFacadeV10")
public class StatisticalOperationsRestFacadeV10Impl implements StatisticalOperationsRestFacadeV10 {

    @Autowired
    private StatisticalOperationsBaseService statisticalOperationsBaseService;

    @Autowired
    private Do2RestInternalMapperV10         do2RestInternalMapper;

    @Context
    private MessageContext                   context;

    private ServiceContext                   serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal");
    private Logger                           logger                     = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Override
    public Operations findOperations() {
        return null; // TODO findOperations
    }

    @Override
    public Response retrieveOperationByCode(String code) {
        try {
            // TODO Validation of parameters

            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestInternal, code);
            if (operationEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationEntity.getProcStatus()))) {
                // Operation not found
                Error error = getError(ServiceExceptionType.OPERATION_NOT_FOUND, code);
                return Response.status(Status.NOT_FOUND).entity(error).build();
            }

            // Transform and return
            Operation operation = do2RestInternalMapper.toOperation(operationEntity, getApiUrl());
            return Response.ok(operation).build();

        } catch (MetamacException e) {
            return throwResponseError(e);
        }
    }

    // TODO pasar a librería común
    private Error getError(CommonServiceExceptionType exceptionType, String parameter) {
        Error error = new Error();
        ErrorItem errorItem = new ErrorItem();
        errorItem.setCode(exceptionType.toString());
        if (parameter != null) {
            errorItem.getParameters().add(parameter);
        }
        error.getErrorItems().add(errorItem);
        return error;
    }

    // TODO pasar a librería común
    /**
     * Get Base API url
     */
    private String getApiUrl() {
        UriInfo uriInfo = context.getUriInfo();
        URI uri = uriInfo.getBaseUri();
        return uri.toString();
    }    

    /**
     * Throws response error, logging exception
     */
    private Response throwResponseError(MetamacException e) {
        logger.error("Error", e);
        Error error = do2RestInternalMapper.toError(e);
        return Response.status(Status.INTERNAL_SERVER_ERROR).entity(error).build();
    }
}