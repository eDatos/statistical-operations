package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.net.URI;
import java.util.List;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ErrorItem;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResourcesPagedResult;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.search.criteria.SculptorCriteria;
import org.siemac.metamac.rest.search.criteria.mapper.RestCriteria2SculptorCriteria;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
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

    // @Context // must inject with setter, because with @Context is not injected in web server
    private MessageContext                   context;

    private ServiceContext                   serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal");
    private Logger                           logger                     = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Context
    public void setMessageContext(MessageContext context) {
        this.context = context;
    }

    @Override
    public Operation retrieveOperationByCode(String code) {
        try {
            // TODO Validation of parameters. validar code o delegar en servicio?

            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedInternalOrExternally(code);

            // Transform
            Operation operation = do2RestInternalMapper.toOperation(operationEntity, getApiUrl());
            return operation;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    // TODO parámetro "query" con criterios de búsqueda METAMAC-753
    @Override
    public RelatedResourcesPagedResult findOperations(String limit, String offset) {
        try {
            // TODO Validation of parameters. delegar en servicio?

            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = RestCriteria2SculptorCriteria.restCriteriaToSculptorCriteria(limit, offset);
            // Find only published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            RelatedResourcesPagedResult operationsPagedResult = do2RestInternalMapper.toOperationsPagedResult(operationsEntitiesResult, sculptorCriteria.getLimit(), getApiUrl());
            return operationsPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public RelatedResourcesNoPagedResult retrieveFamiliesByOperation(String code) {
        try {
            // TODO Validation of parameters. validar code o delegar en servicio?

            // Retrieve operation to check exists and it is published
            retrieveOperationEntityPublishedInternalOrExternally(code);

            // Retrieve families by criteria
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.operations().code()).eq(code).withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY)
                    .distinctRoot().build();
            PagingParameter pagingParameter = PagingParameter.noLimits();
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestInternal,
                    conditionalCriteria, pagingParameter);

            // Transform
            RelatedResourcesNoPagedResult familiesNoPagedResult = do2RestInternalMapper.toFamiliesByOperationNoPagedResult(familiesEntitiesResult.getValues(), getApiUrl());
            return familiesNoPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public Family retrieveFamilyByCode(String code) {
        try {
            // TODO Validation of parameters. validar code o delegar en servicio?

            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = retrieveFamilyEntityPublishedInternalOrExternally(code);

            // Transform
            Family family = do2RestInternalMapper.toFamily(familyEntity, getApiUrl());
            return family;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }
    
    // TODO parámetro "query" con criterios de búsqueda METAMAC-753
    @Override
    public RelatedResourcesPagedResult findFamilies(String limit, String offset) {
        try {
            // TODO Validation of parameters. delegar en servicio?

            // Retrieve families by criteria
            SculptorCriteria sculptorCriteria = RestCriteria2SculptorCriteria.restCriteriaToSculptorCriteria(limit, offset);
            // Find only published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            RelatedResourcesPagedResult familiesPagedResult = do2RestInternalMapper.toFamiliesPagedResult(familiesEntitiesResult, sculptorCriteria.getLimit(), getApiUrl());
            return familiesPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public RelatedResourcesPagedResult retrieveOperationsByFamily(String code, String limit, String offset) {
        try {
            // TODO Validation of parameters. validar code o delegar en servicio?

            // Validate family exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Family family = retrieveFamilyEntityPublishedInternalOrExternally(code);

            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = RestCriteria2SculptorCriteria.restCriteriaToSculptorCriteria(limit, offset);
            // Find only this family and published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.families().code()).eq(code).withProperty(OperationProperties.procStatus())
                    .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            RelatedResourcesPagedResult operationsPagedResult = do2RestInternalMapper.toOperationsByFamilyPagedResult(family, operationsEntitiesResult, sculptorCriteria.getLimit(), getApiUrl());
            return operationsPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public Instance retrieveInstanceByCode(String operationCode, String code) {
        try {
            // TODO Validation of parameters. validar code o delegar en servicio?

            org.siemac.metamac.statistical.operations.core.domain.Instance instanceEntity = statisticalOperationsBaseService.findInstanceByCode(serviceContextRestInternal, code);
            if (instanceEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceEntity.getProcStatus()))) {
                // Instance not found
                Error error = getError(ServiceExceptionType.INSTANCE_NOT_FOUND, code);
                throw new RestException(error, Status.NOT_FOUND);
            }

            // Transform and return
            Instance instance = do2RestInternalMapper.toInstance(instanceEntity, getApiUrl());
            return instance;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    // TODO pasar a librería común?
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

    // TODO pasar a librería común?
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
    private RestException manageException(MetamacException e) {
        logger.error("Error", e);
        Error error = do2RestInternalMapper.toError(e);
        return new RestException(error, Status.INTERNAL_SERVER_ERROR);
    }

    /**
     * Retrieve operation by code and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Operation retrieveOperationEntityPublishedInternalOrExternally(String code) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestInternal, code);
        if (operationEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationEntity.getProcStatus()))) {
            // Operation not found
            Error error = getError(ServiceExceptionType.OPERATION_NOT_FOUND, code);
            throw new RestException(error, Status.NOT_FOUND);
        }
        return operationEntity;
    }

    /**
     * Retrieve family by code and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Family retrieveFamilyEntityPublishedInternalOrExternally(String code) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = statisticalOperationsBaseService.findFamilyByCode(serviceContextRestInternal, code);

        if (familyEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(familyEntity.getProcStatus()))) {
            // Family not found or not published
            Error error = getError(ServiceExceptionType.FAMILY_NOT_FOUND, code);
            throw new RestException(error, Status.NOT_FOUND);
        }
        return familyEntity;
    }
}